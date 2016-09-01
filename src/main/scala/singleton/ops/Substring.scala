package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Substring[A, B] extends Op {
  override type Out <: String
}

object Substring extends Op2Companion[Substring] {

  implicit def materializeConcat[A <: String, B <: Int]: Substring[A, B] =
    macro SubstringMacro.materialize[A, B]

  @ bundle
  final class SubstringMacro(val c: whitebox.Context) extends Macros {
    def materialize[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Tree =
      materializeOp2[Substring, A, B].usingFunction(
        (_: String).substring(_: Int))
  }
}
