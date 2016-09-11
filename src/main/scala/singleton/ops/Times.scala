package singleton.ops

import macrocompat.bundle

import scala.reflect.macros.whitebox
import singleton.ops.impl.{GeneralMacros, _}

trait Times[A, B] extends Op

object Times extends Op2Companion[Times] {

  implicit def materializeTimes[T, A <: T, B <: T](
      implicit nt: Numeric[T]
  ): Times[A, B] = macro TimesMacro.materialize[T, A, B]

  @bundle
  final class TimesMacro(val c: whitebox.Context) extends GeneralMacros {
    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp2[Times, A, B].usingFunction(evalTyped(nt).times)
  }
}
