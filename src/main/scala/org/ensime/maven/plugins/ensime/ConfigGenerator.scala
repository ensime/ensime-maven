/*
 * Copyright 2012 Happy-Camper Street.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package org.ensime.maven.plugins.ensime

import java.io.{ File, FileOutputStream, FileNotFoundException }
import java.nio.file.Files
import java.nio.file.Paths
import java.util.{ List => JList }
import java.util.Properties
import java.util.{ Set => JSet }
import java.util.{ Map => JMap }
import scala.collection.JavaConversions._
import scala.collection.immutable.ListSet
import scala.sys.process._
import scala.util._
import scalax.io.JavaConverters._
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.apache.maven.artifact.Artifact
import org.apache.maven.project.MavenProject
import org.apache.maven.model.Plugin
import org.ensime.maven.plugins.ensime.model.Project
import collection.JavaConverters._
import org.ensime.maven.plugins.ensime.model.SubProject
import org.ensime.maven.plugins.ensime.sexpr.SExpr
import org.ensime.maven.plugins.ensime.sexpr.SExprEmitter
import org.ensime.maven.plugins.ensime.model.FormatterPreferences

/**
 * Represents an ENSIME configuration file generator.
 * @author ueshin
 */
class ConfigGenerator(
    val project: MavenProject,
    val properties: Properties) {

  val jarPattern = "\\.jar$".r

  def getJavaHome(): String = {
    List(
      // manual
      sys.env.get("JDK_HOME"),
      sys.env.get("JAVA_HOME"),
      // fallback
      sys.props.get("java.home").map(new File(_).getParent),
      sys.props.get("java.home"),
      // osx
      Try("/usr/libexec/java_home".!!.trim).toOption).flatten.filter { n =>
        new File(n + "/lib/tools.jar").exists
      }.headOption.getOrElse(
        throw new FileNotFoundException(
          """Could not automatically find the JDK/lib/tools.jar.
      |You must explicitly set JDK_HOME or JAVA_HOME.""".stripMargin))
  }

  /**
   * Get java-flags from environment variable `ENSIME_JAVA_FLAGS` .
   * Used for starting ensime-server.
   * @return List of java flags or empty list if not provided
   * @author parsnips
   */
  def getEnsimeJavaFlags(): List[String] = {
    Option(System.getenv("ENSIME_JAVA_FLAGS")) match {
      case Some(flags) => parser.JavaFlagsParser(flags)
      case _           => List()
    }
  }

  /**
   * Get the scalacOptions for this project.
   * @return A list containing the scalacOptions
   * @author amanjpro
   */
  def getScalacOptions(): List[String] = {
    val scalacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get("net.alchim31.maven:scala-maven-plugin")
    Option(scalacPlugin).map(_.getConfiguration).flatMap {
      case config: Xpp3Dom =>
        Option(config.getChild("args"))
          .map(_.getChildren.toList.map(_.getValue))
    }.toList.flatten
  }

  /**
   * Get the javacOptions for this project.
   * @return A list containing the javacOptions
   * @author amanjpro
   */
  def getJavacOptions(): List[String] = {
    val javacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get("org.apache.maven.plugins:maven-compiler-plugin")
    Option(javacPlugin).map(_.getConfiguration).flatMap {
      case config: Xpp3Dom =>
        Option(config.getChild("compilerArgs"))
          .map(_.getChildren.toList.map(_.getValue))
    }.toList.flatten
  }



  /**
   * Get the scala-version for this project.  Uses scala.version as the key.
   * If you want a blue shed, get out a can of paint :)
   * @return String containing the scala version
   * @author parsnips
   */
  def getScalaVersion(): String = {
    Option(project.getProperties().getProperty("scala.version")).getOrElse("2.10.6") // So arbitrary.
  }

  /**
   * An inefficient scala source root finder.
   * @param lookIn - List of java source roots according to maven
   * @return a list of scala source roots
   * @author parsnips
   */
  def getScalaSourceRoots(lookIn: JList[String]): JList[String] = {
    val scalas = new scala.collection.mutable.SetBuilder[String, Set[String]](Set())

    lookIn.foreach { here =>
      val path = here.split("/").filter(_ != "java").mkString("/") + "/scala"
      if (Files.exists(Paths.get(path))) {
        scalas += path
      }
    }
    scalas.result().toList
  }

  /**
   * Generates configurations.
   */
  def generate(out: File): Unit = {

    implicit val ArtifactToString = new As[Artifact, String] {

      def as(artifact: Artifact) = {
        artifact.getGroupId + ":" + artifact.getArtifactId + ":" + artifact.getVersion
      }
    }

    val modules = (project :: project.getCollectedProjects.asInstanceOf[JList[MavenProject]].toList).filter {
      project => project.getPackaging != "pom"
    }

    val artifactIdToModule = modules.map {
      module => module.getArtifact.as[String] -> module
    }.toMap

    implicit val MavenProjectAsSubProject = new As[MavenProject, SubProject] {

      def as(project: MavenProject): SubProject = {
        val (dependsOnModules, runtimeDeps, compileDeps, testDeps) =
          (project.getArtifacts.asInstanceOf[JSet[Artifact]] :\ (ListSet.empty[MavenProject], List.empty[String], List.empty[String], List.empty[String])) {
            case (artifact, (dependsOnModules, runtimeDeps, compileDeps, testDeps)) =>
              val artifactId = artifact.as[String]
              if (artifactIdToModule.contains(artifactId)) {
                (dependsOnModules + artifactIdToModule(artifactId), runtimeDeps, compileDeps, testDeps)
              } else {
                // make sure we're only loading jars, otherwise ensime will have
                // issues. Not sure if we should include `ejb-client` or not,
                // but definitely throws errors on `pom` files
                if (artifact.getType().matches("(test-)?jar")) {
                  val path = artifact.getFile.getAbsolutePath
                  artifact.getScope match {
                    case Artifact.SCOPE_PROVIDED =>
                      (dependsOnModules, runtimeDeps, path :: compileDeps, path :: testDeps)
                    case Artifact.SCOPE_RUNTIME =>
                      (dependsOnModules, path :: runtimeDeps, compileDeps, path :: testDeps)
                    case Artifact.SCOPE_TEST =>
                      (dependsOnModules, runtimeDeps, compileDeps, path :: testDeps)
                    case _ =>
                      (dependsOnModules, path :: runtimeDeps, path :: compileDeps, path :: testDeps)
                  }
                } else {
                  (dependsOnModules, runtimeDeps, compileDeps, testDeps)
                }
              }
          }

        val sourceRoots = project.getCompileSourceRoots.asInstanceOf[JList[String]] ++: project.getTestCompileSourceRoots.asInstanceOf[JList[String]] ++: Nil
        val scalaRoots = getScalaSourceRoots(sourceRoots)
        val allDeps = (runtimeDeps ++ compileDeps ++ testDeps).toSet

        SubProject(
          project.getArtifactId,
          project.getVersion,
          runtimeDeps ::: ((dependsOnModules.toList :+ project).map { module =>
            module.getBuild.getOutputDirectory
          }),
          compileDeps ::: (dependsOnModules.toList.map { module =>
            module.getBuild.getOutputDirectory
          }),
          testDeps,
          // TODO: File a bug against ensime-server, cuz this breaks shit
          // ::: ((dependsOnModules.toList :+ project).map { module =>
          //module.getBuild.getOutputDirectory
          //} :+ project.getBuild.getTestOutputDirectory),
          scalaRoots ++: sourceRoots,
          List(project.getBuild.getOutputDirectory),
          List(project.getBuild.getTestOutputDirectory),
          dependsOnModules.toList.map(_.getArtifactId),
          allDeps.foldLeft(List[String]()) { (output, dep) =>
            val sourceJar = jarPattern.replaceFirstIn(dep, "-sources.jar")
            if (new java.io.File(sourceJar).exists) {
              sourceJar :: output
            } else {
              output
            }
          },
          allDeps.foldLeft(List[String]()) { (output, dep) =>
            val docJar = jarPattern.replaceFirstIn(dep, "-javadoc.jar")
            if (new java.io.File(docJar).exists) {
              docJar :: output
            } else {
              output
            }
          })
      }
    }

    val projectDir = project.getBasedir().toPath().toAbsolutePath().toString()
    val cacheDir = projectDir + "/.ensime_cache"
    val emitter = new SExprEmitter(Project(project.getName(), projectDir, cacheDir, getScalaVersion(), getJavaHome(), getEnsimeJavaFlags(), modules.map(_.as[SubProject]), FormatterPreferences(properties)).as[SExpr])
    emitter.emit(new FileOutputStream(out).asOutput)
  }
}
