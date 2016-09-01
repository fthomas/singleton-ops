package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Times[T, A, B] extends Op {
  override type Out <: T
}

object Times extends Op2CompanionGen[Times] {

  implicit def materializeTimes[T, A <: T, B <: T](
      implicit nt: Numeric[T]
  ): Times[T, A, B] = macro TimesMacro.materialize[T, A, B]

  @ bundle
  final class TimesMacro(val c: whitebox.Context) extends Macros {
    def materialize[T: c.WeakTypeTag, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp2Gen[Times, T, A, B].usingFunction(evalTyped(nt).times)
  }
}
