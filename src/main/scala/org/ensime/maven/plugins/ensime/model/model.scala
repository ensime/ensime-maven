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

import java.io.File

case class EnsimeProjectId(
  val project: String,
  val config: String)

case class EnsimeProject(
  val id: EnsimeProjectId,
  val depends: Seq[EnsimeProjectId],
  val sources: Set[File],
  val targets: Set[File],
  val scalacOptions: List[String],
  val javacOptions: List[String],
  val libraryJars: Set[File],
  val librarySources: Set[File],
  val libraryDocs: Set[File])

case class EnsimeModule(
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
    val docJars: Set[File]) {
  def dependencies(implicit lookup: String => EnsimeModule): Set[EnsimeModule] =
    dependsOnNames map lookup
}

case class EnsimeConfig(
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
  val projects: Seq[EnsimeProject])

