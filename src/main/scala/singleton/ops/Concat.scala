package singleton.ops

import macrocompat.bundle

import scala.reflect.macros.whitebox
import singleton.ops.impl.{GeneralMacros, _}

trait Concat[A, B] extends Op

object Concat extends Op2Companion[Concat] {

  implicit def materializeConcat[A <: String, B <: String]: Concat[A, B] =
    macro ConcatMacro.materialize[A, B]

  @bundle
  final class ConcatMacro(val c: whitebox.Context) extends GeneralMacros {
    def materialize[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Tree =
      materializeOp2[Concat, A, B].usingFunction((_: String) + (_: String))
  }
}
