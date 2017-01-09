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

import java.io.{ PrintWriter, File, FileOutputStream, FileNotFoundException }
import java.util.{ List => JList }
import java.util.{ Set => JSet }
import java.util.{ Map => JMap }
import java.util.Properties
import scala.sys.process._
import scala.util._
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.apache.maven.artifact.Artifact
import org.apache.maven.project.MavenProject
import org.apache.maven.model.{Plugin, Repository}
import org.eclipse.aether.{RepositorySystemSession, RepositorySystem}
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.resolution.{ArtifactRequest,
                                      ArtifactResult,
                                      DependencyRequest,
                                      ArtifactResolutionException}
import org.eclipse.aether.collection.CollectRequest
import org.eclipse.aether.graph.Dependency
import org.eclipse.aether.repository.RemoteRepository
import scala.collection.JavaConverters._
import org.ensime.maven.plugins.ensime.model._
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

  implicit class StrintToPath(path: String) {
    def /(content: String) = path + File.separator + content
  }

  private val remoteRepositories = {
    val repos = project.getRepositories
      .asInstanceOf[JList[Repository]].asScala.toList
    repoSystem.newResolutionRepositories(session,
      repos.map(r =>
        new RemoteRepository.Builder(r.getId, "default", r.getUrl).build).asJava)
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

  private def getJavaHome(): File = {
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
    resolveAll(artifact("org.ensime", s"server_$scala", ensimeServerVersion)) +
      resolve(artifact(org, "scalap", systemScalaVersion))
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

  private lazy val getScalaJars =
    resolveScalaJars(getScalaOrganization, getScalaVersion)

  private def getEnsimeServerJars() =
    resolveEnsimeJars(getScalaOrganization, ensimeServerVersion) --
      getScalaJars +
      new File(getJavaHome.getAbsolutePath / "lib" / "tools.jar")

  /**
   * Get java-flags from environment variable `ENSIME_JAVA_FLAGS` .
   * Used for starting ensime-server.
   * @return List of java flags or empty list if not provided
   * @author parsnips
   */
  private def getEnsimeJavaFlags(): List[String] = {
    Option(System.getenv("ENSIME_JAVA_FLAGS")).map(flags =>
      parser.JavaFlagsParser(flags)).toList.flatten
  }

  /**
   * Get the Scala organization for this project.
   * @return String containing the scala organization
   * @author amanjpro
   */
  private def getScalaOrganization(): String = {
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
  private def getScalacOptions(project: MavenProject): List[String] = {
    val scalacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get(SCALA_MAVEN_PLUGIN)
    val providedOptions = Option(scalacPlugin).map(_.getConfiguration).flatMap {
      case config: Xpp3Dom =>
        Option(config.getChild("args"))
          .map(_.getChildren.toList.map(_.getValue))
    }.toList.flatten

    val suggestedOptions = Set(
      "-feature",
      "-deprecation",
      "-Xlint",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Xfuture") ++ {
        partialVersion match {
          case (2, 10) =>
            Set("-Ymacro-no-expand")
          case (2, v) if v >= 11 =>
            Set("-Ywarn-unused-import", "-Ymacro-expand:discard")
          case _ => Set.empty
        }
      }

    providedOptions ++ suggestedOptions
  }

  /**
   * Get the javacOptions for this project.
   * @return A list containing the javacOptions
   * @author amanjpro
   */
  private def getJavacOptions(project: MavenProject): List[String] = {
    val javacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get(JAVA_MAVEN_PLUGIN)

    val scalacPlugin =
      project.getPluginManagement().getPluginsAsMap
        .asInstanceOf[JMap[String, Plugin]]
        .get(SCALA_MAVEN_PLUGIN)

    val javacOptions = Option(javacPlugin).map(_.getConfiguration).flatMap {
      case config: Xpp3Dom =>
        Option(config.getChild("compilerArgs"))
          .map(_.getChildren.toList.map(_.getValue))
    }.toList.flatten

    val jvmOptions = Option(scalacPlugin).map(_.getConfiguration).flatMap {
      case config: Xpp3Dom =>
        Option(config.getChild("jvmArgs"))
          .map(_.getChildren.toList.map(_.getValue))
    }.toList.flatten

    javacOptions ++ jvmOptions

  }

  /**
   * Get the scala-version for this project.  Uses scala.version as the key.
   * If you want a blue shed, get out a can of paint :)
   * @return String containing the scala version
   * @author parsnips
   */
  private def getScalaVersion(): String = {
    Option(project.getProperties().getProperty("scala.version")).getOrElse("2.10.6") // So arbitrary.
  }

  private def getEnsimeProjects(): List[EnsimeProject] = {
    val modules = (project :: project.getCollectedProjects.asInstanceOf[JList[MavenProject]].asScala.toList).filter {
      project => project.getPackaging != "pom"
    }

    modules.map { module =>
      val projectId = EnsimeProjectId(project.getId, Option(project.getDefaultGoal).getOrElse("compile"))
      val dependencyArtifacts =
        project.getDependencyArtifacts.asInstanceOf[JSet[Artifact]].asScala.toSet

      // This only gets the direct dependencies, and we filter all the
      // dependencies that are not a subproject of this potentially
      // multiproject project
      val depends = dependencyArtifacts.filter(d => modules.exists(m => m.getId == d.getId)).toSeq.map(d => EnsimeProjectId(d.getId, "compile"))

      val sources = {
        val scalacPlugin =
          project.getPluginManagement().getPluginsAsMap
            .asInstanceOf[JMap[String, Plugin]]
            .get(SCALA_MAVEN_PLUGIN)

        val compileSources = {
          val scalaSources = {
            val sources = Option(scalacPlugin).map(_.getConfiguration).flatMap {
              case config: Xpp3Dom =>
                Option(config.getChild("sources"))
                  .map(_.getChildren.toList.map(_.getValue))
            }.toList.flatten
            if (sources == Nil) {
              Set(new File(module.getBasedir.getAbsolutePath / "src" / "main" / "scala").getAbsolutePath)
            } else sources
          }

          (scalaSources ++
            module.getCompileSourceRoots.asInstanceOf[JList[String]].asScala).toSet
        }
        val testSources = {
          val scalaTests = {
            val tests = Option(scalacPlugin).map(_.getConfiguration).flatMap {
              case config: Xpp3Dom =>
                Option(config.getChild("sources"))
                  .map(_.getChildren.toList.map(_.getValue))
            }.toList.flatten
            if (tests == Nil) {
              Set(new File(module.getBasedir.getAbsolutePath / "src" / "test" / "scala").getAbsolutePath)
            } else tests
          }
          (scalaTests ++ module.getTestCompileSourceRoots.asInstanceOf[JList[String]].asScala).toSet
        }
        (compileSources ++ testSources).map(new File(_))
      }
      val targets = Set(new File(project.getBuild.getOutputDirectory))
      val scalacOptions = getScalacOptions(project)
      val javacOptions = getJavacOptions(project)

      val libraryJars = dependencyArtifacts.flatMap { art =>
        resolveAll(new DefaultArtifact(art.getGroupId,
          art.getArtifactId, "jar", art.getVersion))
      }

      val librarySources = dependencyArtifacts.flatMap { art =>
        resolveAll(new DefaultArtifact(art.getGroupId,
          art.getArtifactId, "sources", "jar", art.getVersion))
      }

      val libraryDocs = dependencyArtifacts.flatMap { art =>
        resolveAll(new DefaultArtifact(art.getGroupId,
          art.getArtifactId, "javadoc", "jar", art.getVersion))
      }

      EnsimeProject(projectId, depends, sources, targets,
        scalacOptions, javacOptions, libraryJars, librarySources,
        libraryDocs)
    }
  }

  private def write(content: String, out: File) = {
    val writer = new PrintWriter(out)
    writer.write(content)
    writer.close
  }

  /**
   * Generates configurations.
   */
  def generate(out: File): Unit = {

    val projectDir = project.getBasedir().toPath().toAbsolutePath().toString()
    val cacheDir = new File(projectDir / ".ensime_cache")

    val subProjects = getEnsimeProjects

    val modules = subProjects.groupBy(_.id.project).mapValues(ensimeProjectsToModule)
    val javaSrc = {
      val file = new File(getJavaHome.getAbsolutePath / "src.zip")
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
    // val emitter = new SExprEmitter(config.as[SExpr])
    // emitter.emit(new FileOutputStream(out).asOutput)
    write(SExpFormatter.toSExp(config).replaceAll("\r\n", "\n") + "\n", out)
  }
}

// direct formatter to deal with a small number of domain objects
// if we had to do this for general objects, it would make sense
// to create a series of implicit convertors to an SExp hierarchy
object SExpFormatter {

  // normalise and ensure monkeys go first
  // (bit of a hack to do it here, maybe best when creating)
  private[ensime] def orderFiles(ss: Iterable[File]): List[File] = {
    val (monkeys, humans) = ss.toList.distinct.sortBy { f =>
      f.getName + f.getPath
    }.partition(_.getName.contains("monkey"))
    monkeys ::: humans
  }

  private def toSExp(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def toSExp(f: File): String = toSExp(f.getAbsolutePath)

  private def fsToSExp(ss: Iterable[File]): String =
    if (ss.isEmpty) "nil"
    else orderFiles(ss).map(toSExp).mkString("(", " ", ")")

  private def ssToSExp(ss: Iterable[String]): String =
    if (ss.isEmpty) "nil"
    else ss.toSeq.map(toSExp).mkString("(", " ", ")")

  private def msToSExp(ss: Iterable[EnsimeModule]): String =
    if (ss.isEmpty) "nil"
    else ss.toSeq.sortBy(_.name).map(toSExp).mkString("(", " ", ")")

  private def psToSExp(ss: Iterable[EnsimeProject]): String =
    if (ss.isEmpty) "nil"
    else ss.toSeq.sortBy(_.id.toString).map(toSExp).mkString("(", " ", ")")

  private def fToSExp(key: String, op: Option[File]): String =
    op.map { f => s":$key ${toSExp(f)}" }.getOrElse("")

  private def sToSExp(key: String, op: Option[String]): String =
    op.map { f => s":$key ${toSExp(f)}" }.getOrElse("")

  private def toSExp(b: Boolean): String = if (b) "t" else "nil"

  // a lot of legacy key names and conventions
  def toSExp(c: EnsimeConfig): String = s"""(
 :root-dir ${toSExp(c.root)}
 :cache-dir ${toSExp(c.cacheDir)}
 :scala-compiler-jars ${fsToSExp(c.scalaCompilerJars)}
 :ensime-server-jars ${fsToSExp(c.ensimeServerJars)}
 :name "${c.name}"
 :java-home ${toSExp(c.javaHome)}
 :java-flags ${ssToSExp(c.javaFlags)}
 :java-sources ${fsToSExp(c.javaSrc)}
 :java-compiler-args ${ssToSExp(c.javacOptions)}
 :reference-source-roots ${fsToSExp(c.javaSrc)}
 :scala-version ${toSExp(c.scalaVersion)}
 :compiler-args ${ssToSExp(c.scalacOptions)}
 :subprojects ${msToSExp(c.modules.values)}
 :projects ${psToSExp(c.projects)}
)"""

  // a lot of legacy key names and conventions
  private def toSExp(m: EnsimeModule): String = s"""(
   :name ${toSExp(m.name)}
   :source-roots ${fsToSExp((m.mainRoots ++ m.testRoots))}
   :targets ${fsToSExp(m.targets)}
   :test-targets ${fsToSExp(m.testTargets)}
   :depends-on-modules ${ssToSExp(m.dependsOnNames.toList.sorted)}
   :compile-deps ${fsToSExp(m.compileJars)}
   :runtime-deps ${fsToSExp(m.runtimeJars)}
   :test-deps ${fsToSExp(m.testJars)}
   :doc-jars ${fsToSExp(m.docJars)}
   :reference-source-roots ${fsToSExp(m.sourceJars)})"""

  private def toSExp(p: EnsimeProject): String = s"""(
    :id ${toSExp(p.id)}
    :depends ${idsToSExp(p.depends)}
    :sources ${fsToSExp(p.sources)}
    :targets ${fsToSExp(p.targets)}
    :scalac-options ${ssToSExp(p.scalacOptions)}
    :javac-options ${ssToSExp(p.javacOptions)}
    :library-jars ${fsToSExp(p.libraryJars)}
    :library-sources ${fsToSExp(p.librarySources)}
    :library-docs ${fsToSExp(p.libraryDocs)})"""

  private def toSExp(id: EnsimeProjectId): String =
    s"""(:project ${toSExp(id.project)} :config ${toSExp(id.config)})"""

  private def idsToSExp(ids: Iterable[EnsimeProjectId]): String =
    if (ids.isEmpty) "nil"
    else ids.toSeq.sortBy(_.toString).map(toSExp).mkString("(", " ", ")")

}
