package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Plus[A, B] extends Op

object Plus extends Op2Companion[Plus] {

  implicit def materializePlus[T, A <: T, B <: T](
      implicit nt: Numeric[T]
  ): Plus[A, B] = macro PlusMacro.materialize[T, A, B]

  @bundle
  final class PlusMacro(val c: whitebox.Context) extends Macros {
    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp2[Plus, A, B].usingFunction(evalTyped(nt).plus)
  }
}
