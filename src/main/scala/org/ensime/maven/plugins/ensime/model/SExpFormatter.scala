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
