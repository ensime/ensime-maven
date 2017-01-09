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
package sexpr

import scalax.io.Codec
import scalax.io.Output

/**
 * Represents S-Expression emitter.
 * @author ueshin
 */
class SExprEmitter(val sexpr: SExpr) {

  /**
   * Emits S-Expression.
   */
  def emit(output: Output)(implicit codec: Codec = Codec.default): Unit = {
    for (processor <- output.outputProcessor) {
      emitSExpr(processor.asOutput, sexpr)
    }
  }

  @inline
  private def emitSExpr(out: Output, sexpr: SExpr, indent: Int = 0): Unit = sexpr match {
    case SString(value)    => emitSString(out, value)
    case SInt(value)       => emitSInt(out, value)
    case STrue             => emitSTrue(out)
    case SNil              => emitSNil(out)
    case SKeyword(keyword) => emitSKeyword(out, keyword)
    case SList(list)       => emitSList(out, list, indent)
    case SMap(map)         => emitSMap(out, map, indent)
  }

  @inline
  private def emitSString(out: Output, value: String) = {
    val newValue = "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    out.write(newValue)
  }

  @inline
  private def emitSInt(out: Output, value: Int) = out.write(value.toString)

  @inline
  private def emitSTrue(out: Output) = out.write("t")

  @inline
  private def emitSNil(out: Output) = out.write("nil")

  @inline
  private def emitSKeyword(out: Output, keyword: String) = out.write(":" + keyword)

  @inline
  private def emitSList(out: Output, list: Seq[SExpr], indent: Int) = {
    if (list.isEmpty) {
      emitSNil(out)
    } else {
      out.write("(")
      list.headOption.foreach { head =>
        emitSExpr(out, head, indent + 1)
        list.tail.foreach { sexpr =>
          out.write("\n")
          out.write(" " * (indent + 1))
          emitSExpr(out, sexpr, indent + 1)
        }
      }
      out.write(")")
    }
  }

  @inline
  private def emitSMap(out: Output, map: Seq[(SKeyword, SExpr)], indent: Int) = {
    out.write("(")
    map.headOption.foreach {
      case (SKeyword(key), value) =>
        emitSKeyword(out, key)
        out.write("\n")
        out.write(" " * (indent + 3))
        emitSExpr(out, value, indent + 3)
        map.tail.foreach {
          case (SKeyword(key), value) =>
            out.write("\n")
            out.write(" " * (indent + 1))
            emitSKeyword(out, key)
            out.write("\n")
            out.write(" " * (indent + 3))
            emitSExpr(out, value, indent + 3)
        }
    }
    out.write(")")
  }
}
