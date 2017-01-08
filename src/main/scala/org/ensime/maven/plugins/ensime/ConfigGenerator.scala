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
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.resolution.ArtifactRequest
import org.eclipse.aether.resolution.ArtifactResult
import org.eclipse.aether.resolution.DependencyRequest
import org.eclipse.aether.resolution.ArtifactResolutionException
import org.eclipse.aether.collection.CollectRequest
import org.eclipse.aether.graph.Dependency
import org.eclipse.aether.repository.RemoteRepository
import org.apache.maven.model.Plugin
import org.apache.maven.model.Repository
import collection.JavaConverters._
import org.ensime.maven.plugins.ensime.model._
import org.ensime.maven.plugins.ensime.sexpr.SExpr
import org.ensime.maven.plugins.ensime.sexpr.SExprEmitter
import org.ensime.maven.plugins.ensime.model.FormatterPreferences
import scala.util.Properties.{ versionNumberString => systemScalaVersion }

/**
 * Represents an ENSIME configuration file generator.
 * @author ueshin
 */
class ConfigGenerator(
    val project: MavenProject,
    val repoSystem: RepositorySystem,
    val session: RepositorySystemSession,
    val properties: Properties) {

  private val remoteRepositories = {
    val repos = project.getRepositories
      .asInstanceOf[JList[Repository]].asScala.toList
    repoSystem.newResolutionRepositories(session,
      repos.map(r =>
        new RemoteRepository.Builder(r.getId, "default", r.getUrl).build))
  }

  private val SCALA_MAVEN_PLUGIN_GROUP_ID =
    "net.alchim31.maven"
  private val SCALA_MAVEN_PLUGIN_ARTIFACT_ID =
    "scala-maven-plugin"

  private val JAVA_MAVEN_PLUGIN_GROUP_ID =
    "org.apache.maven.plugins"
  private val JAVA_MAVEN_PLUGIN_ARTIFACT_ID =
    "maven-compiler-plugin"

  private val SCALA_MAVEN_PLUGIN =
    s"${SCALA_MAVEN_PLUGIN_GROUP_ID}:${SCALA_MAVEN_PLUGIN_ARTIFACT_ID}"

  private val JAVA_MAVEN_PLUGIN =
    s"${JAVA_MAVEN_PLUGIN_GROUP_ID}:${JAVA_MAVEN_PLUGIN_ARTIFACT_ID}"

  private val ensimeServerVersion = "1.0.0"

  def getJavaHome(): File = {
    List(
      // manual
      sys.env.get("JDK_HOME"),
      sys.env.get("JAVA_HOME"),
      // fallback
      sys.props.get("java.home").map(new File(_).getParent),
      sys.props.get("java.home"),
      // osx
      Try("/usr/libexec/java_home".!!.trim).toOption).flatten.flatMap { n =>
        val f = new File(n + "/lib/tools.jar")
        if (f.exists)
          List(new File(n))
        else Nil
      }.headOption.getOrElse(
        throw new FileNotFoundException(
          """Could not automatically find the JDK/lib/tools.jar.
      |You must explicitly set JDK_HOME or JAVA_HOME.""".stripMargin))
  }

  private def artifact(groupId: String, artifactId: String, version: String) =
    new DefaultArtifact(groupId, artifactId, "jar", version)

  private def artifactRequest(art: DefaultArtifact) =
    new ArtifactRequest(art, remoteRepositories, null)

  private def resolve(art: DefaultArtifact) =
    repoSystem.resolveArtifact(session,
      artifactRequest(art)).getArtifact.getFile

  private def resolveAll(art: DefaultArtifact) = {
    val dependency = new Dependency(art, "compile")

    val collectRequest = new CollectRequest(dependency, remoteRepositories)

    val node = repoSystem.collectDependencies(session, collectRequest).getRoot()

    val dependencyRequest = new DependencyRequest()
    dependencyRequest.setRoot(node)

    repoSystem.resolveDependencies(session, dependencyRequest)
      .getArtifactResults.asInstanceOf[JList[ArtifactResult]]
      .asScala.map(_.getArtifact.getFile).toSet
  }

  private def partialVersion() = {
    val parts = systemScalaVersion.split("\\.")
    (parts(0).toInt, parts(1).toInt)
  }

  private def resolveScalaJars(org: String, version: String): Set[File] =
    Set(
      resolve(artifact(org, "scalap", version)),
      resolve(artifact(org, "scala-compiler", version)),
      resolve(artifact(org, "scala-library", version)),
      resolve(artifact(org, "scala-reflect", version)))

  private def resolveEnsimeJars(org: String, ensime: String): Set[File] = {
    val scala = {
      val (major, minor) = partialVersion
      s"$major.$minor"
    }
    resolveAll(artifact(org, "scalap", systemScalaVersion)) +
      resolve(artifact("org.ensime", s"server_$scala", ensimeServerVersion))
  }

  private def ensimeProjectsToModule(p: Iterable[EnsimeProject]): EnsimeModule = {
    val name = p.head.id.project
    val deps = for {
      s <- p
      d <- s.depends
    } yield d.project
    val (mains, tests) = p.toSet.partition(_.id.config == "compile")
    val mainSources = mains.flatMap(_.sources)
    val mainTargets = mains.flatMap(_.targets)
    val mainJars = mains.flatMap(_.libraryJars)
    val testSources = tests.flatMap(_.sources)
    val testTargets = tests.flatMap(_.targets)
    val testJars = tests.flatMap(_.libraryJars).toSet -- mainJars
    val sourceJars = p.flatMap(_.librarySources).toSet
    val docJars = p.flatMap(_.libraryDocs).toSet
    EnsimeModule(
      name, mainSources, testSources, mainTargets, testTargets, deps.toSet,
      mainJars, Set.empty, testJars, sourceJars, docJars)
  }

  def getScalaJars() =
    resolveScalaJars(getScalaOrganization, getScalaVersion)

  def getEnsimeServerJars() =
    resolveEnsimeJars(getScalaOrganization, ensimeServerVersion)

  /**
   * Get java-flags from environment variable `ENSIME_JAVA_FLAGS` .
   * Used for starting ensime-server.
   * @return List of java flags or empty list if not provided
   * @author parsnips
   */
  def getEnsimeJavaFlags(): List[String] = {
    val providedFlags = Option(System.getenv("ENSIME_JAVA_FLAGS")) match {
      case Some(flags) => parser.JavaFlagsParser(flags)
      case _           => List()
    }

    val suggestedFlags = Seq("-Densime.config=.ensime", "-Densime.exitAfterIndex=true")
    providedFlags ++ suggestedFlags
  }

  /**
   * Get the Scala organization for this project.
   * @return String containing the scala organization
   * @author amanjpro
   */
  def getScalaOrganization(): String = {
    val scalacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get(SCALA_MAVEN_PLUGIN)
    Option(scalacPlugin).map(_.getConfiguration).flatMap {
      case config: Xpp3Dom =>
        Option(config.getChild("scalaOrganization")).map(_.getValue)
    }.getOrElse("org.scala-lang")
  }

  /**
   * Get the scalacOptions for this project.
   * @return A list containing the scalacOptions
   * @author amanjpro
   */
  def getScalacOptions(project: MavenProject): List[String] = {
    val scalacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get(SCALA_MAVEN_PLUGIN)
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
  def getJavacOptions(project: MavenProject): List[String] = {
    val javacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get(JAVA_MAVEN_PLUGIN)
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

  def getEnsimeProjects(): List[EnsimeProject] = {
    val modules = (project :: project.getCollectedProjects.asInstanceOf[JList[MavenProject]].toList).filter {
      project => project.getPackaging != "pom"
    }

    modules.map { module =>
      val projectId = EnsimeProjectId(project.getId, Option(project.getDefaultGoal).getOrElse("compile"))
      // This only gets the direct dependencies
      val dependencyArtifacts = project.getDependencyArtifacts.asInstanceOf[JSet[Artifact]].asScala.toSet
      val depends = dependencyArtifacts.toSeq.map(d => EnsimeProjectId(d.getId, "compile"))
      val sources = {
        val compileSources =
          module.getCompileSourceRoots.asInstanceOf[JList[String]].asScala.toSet
        val testSources =
          module.getTestCompileSourceRoots.asInstanceOf[JList[String]].asScala.toSet
        (compileSources ++ testSources).map(new File(_))
      }
      val targets = Set(new File(project.getBuild.getOutputDirectory))
      val scalacOptions = getScalacOptions(project)
      val javacOptions = getJavacOptions(project)

      val (libraryJars, librarySources, libraryDocs) =
        dependencyArtifacts.map { art =>
          val jarFile = resolve(new DefaultArtifact(art.getGroupId,
            art.getArtifactId, "jar", art.getVersion))
          val sourcesFile = resolve(new DefaultArtifact(art.getGroupId,
            art.getArtifactId, "sources", "jar", art.getVersion))
          val libraryDocs = resolve(new DefaultArtifact(art.getGroupId,
            art.getArtifactId, "javadoc", "jar", art.getVersion))
          (jarFile, sourcesFile, libraryDocs)
        }.unzip3

      EnsimeProject(projectId, depends, sources, targets,
        scalacOptions, javacOptions, libraryJars, librarySources,
        libraryDocs)
    }
  }

  /**
   * Generates configurations.
   */
  def generate(out: File): Unit = {

    val projectDir = project.getBasedir().toPath().toAbsolutePath().toString()
    val cacheDir = new File(projectDir + "/.ensime_cache")

    val subProjects = getEnsimeProjects

    val modules = subProjects.groupBy(_.id.project).mapValues(ensimeProjectsToModule)
    val javaSrc = {
      val file = new File(getJavaHome + File.separator + "src.zip")
      file match {
        case f if f.exists => Set(f)
        case _             => Set.empty[File]
      }
    }

    val config = EnsimeConfig(project.getBasedir, cacheDir,
      getScalaJars, getEnsimeServerJars, project.getName,
      getScalaVersion(),
      getScalacOptions(project), modules, getJavaHome(),
      getEnsimeJavaFlags(), getJavacOptions(project), javaSrc, subProjects)
    val emitter = new SExprEmitter(config.as[SExpr])
    emitter.emit(new FileOutputStream(out).asOutput)
  }
}
