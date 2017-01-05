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
package model

import org.ensime.maven.plugins.ensime.sexpr._
import org.ensime.maven.plugins.ensime.sexpr.SMap
import org.ensime.maven.plugins.ensime.sexpr.SList
import org.ensime.maven.plugins.ensime.sexpr.SKeyword
import java.io.File


/**
 * Represents ENSIME projectId.
 * @author amanjpro
 */
class EnsimeProjectId(
  val project: String,
  val config: String
)

/**
 * A companion object for {@link EnsimeProjectId}.
 * @author amanjpro
 */
object EnsimeProjectId {

  /**
   * Treats EnsimeProjectId as SExpr.
   */
  implicit object EnsimeProjectIdAsSExpr extends As[EnsimeProjectId, SExpr] {

    override def as(projectId: EnsimeProjectId) = {
      SMap(Seq(
        (SKeyword("id") -> SString(projectId.project)),
        (SKeyword("config") -> SString(projectId.config))))
    }
  }
}

/**
 * Represents ENSIME project.
 * @author amanjpro
 */
class EnsimeProject(
  val id: EnsimeProjectId,
  val depends: Seq[EnsimeProjectId],
  val sources: Set[File],
  val targets: Set[File],
  val scalacOptions: List[String],
  val javacOptions: List[String],
  val libraryJars: Set[File],
  val librarySources: Set[File],
  val libraryDocs: Set[File]
)

/**
 * A companion object for {@link EnsimeProject}.
 * @author amanjpro
 */
object EnsimeProject {
  import EnsimeProjectId._

  /**
   * Treats EnsimeProject as SExpr.
   */
  implicit object EnsimeProjectAsSExpr extends As[EnsimeProject, SExpr] {

    override def as(project: EnsimeProject) = {
      SMap(Seq(
        (SKeyword("id") -> EnsimeProjectIdAsSExpr.as(project.id)),
        (SKeyword("depends") -> SList(project.depends.map(EnsimeProjectIdAsSExpr.as))),
        (SKeyword("sources") -> SList(project.sources.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("targets") -> SList(project.targets.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("scalac-options") -> SList(project.scalacOptions.map(SString))),
        (SKeyword("javac-options") -> SList(project.javacOptions.map(SString))),
        (SKeyword("library-jars"),
          SList(project.libraryJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("library-sources"),
          SList(project.librarySources.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("library-docs"),
          SList(project.libraryDocs.map(f => SString(f.getAbsolutePath)).toSeq))))
    }
  }
}

// 1.0
/**
 * Represents ENSIME module.
 * @author amanjpro
 */
class EnsimeModule(
  val name: String,
  val mainRoots: Set[File],
  val testRoots: Set[File],
  val targets: Set[File],
  val testTargets: Set[File],
  val dependsOnNames: Set[String],
  val compileJars: Set[File],
  val runtimeJars: Set[File],
  val testJars: Set[File],
  val sourceJars: Set[File],
  val docJars: Set[File],
  val dependencies: Set[EnsimeModule]
)
/**
 * A companion object for {@link EnsimeModule}.
 * @author amanjpro
 */
object EnsimeModule {

  /**
   * Treats EnsimeModule as SExpr.
   */
  implicit object EnsimeModuleAsSExpr extends As[EnsimeModule, SExpr] {

    override def as(module: EnsimeModule) = {
      SMap(Seq(
        (SKeyword("name") -> SString(module.name)),
        (SKeyword("mainRoots") -> SList(module.mainRoots.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("testRoots") -> SList(module.testRoots.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("targets") -> SList(module.targets.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("testTargets") ->
          SList(module.testTargets.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("dependsOnNames") ->
          SList(module.dependsOnNames.map(SString).toSeq)),
        (SKeyword("compileJars") ->
          SList(module.compileJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("runtimeJars") ->
          SList(module.runtimeJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("testJars") ->
          SList(module.testJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("sourceJars") ->
          SList(module.sourceJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("docJars") ->
          SList(module.docJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("dependencies") ->
          SList(module.dependencies.map(as).toSeq))))
      }
  }
}

/**
 * Represents ENSIME configuration.
 * @author amanjpro
 */
class EnsimeConfig(
  val root: File,
  val cacheDir: File,
  val scalaCompilerJars: Set[File],
  val ensimeServerJars: Set[File],
  val name: String,
  val scalaVersion: String,
  val scalacOptions: List[String], // 1.0
  val modules: Map[String, EnsimeModule], // 1.0
  val javaHome: File,
  val javaFlags: List[String],
  val javacOptions: List[String], // 1.0
  val javaSrc: Set[File],
  val projects: Seq[EnsimeProject]
)

/**
 * A companion object for {@link EnsimeConfig}.
 * @author amanjpro
 */
object EnsimeConfig {

  /**
   * Treats EnsimeConfig as SExpr.
   */
  implicit object EnsimeConfigAsSExpr extends As[EnsimeConfig, SExpr] {
    import EnsimeProject._
    import EnsimeModule._

    override def as(config: EnsimeConfig) = {
      SMap(Seq(
        (SKeyword("root") -> SString(config.root.getAbsolutePath)),
        (SKeyword("cacheDir") -> SString(config.cacheDir.getAbsolutePath)),
        (SKeyword("scalaCompilerJars") ->
          SList(config.scalaCompilerJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("ensimeServerJars") ->
          SList(config.ensimeServerJars.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("name") -> SString(config.name)),
        (SKeyword("scalaVersion") -> SString(config.scalaVersion)),
        (SKeyword("scalacOptions"), SList(config.scalacOptions.map(SString))),
        (SKeyword("modules") -> SMap(config.modules.map { case (key, value) =>
          (SKeyword(key), EnsimeModuleAsSExpr.as(value)) }.toSeq)),
        (SKeyword("javaHome"), SString(config.javaHome.getAbsolutePath)),
        (SKeyword("javaFlags"), SList(config.javaFlags.map(SString))),
        (SKeyword("javacOptions"), SList(config.javacOptions.map(SString))),
        (SKeyword("javaSrc"), SList(config.javaSrc.map(f => SString(f.getAbsolutePath)).toSeq)),
        (SKeyword("projects"), SList(config.projects.map(EnsimeProjectAsSExpr.as).toSeq))))
    }
  }
}
