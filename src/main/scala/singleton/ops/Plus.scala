package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.macros.MacroUtils

trait Plus[A, B] {
  type Out
}

object Plus {
  type Aux[A, B, Out0] = Plus[A, B] { type Out = Out0 }

  def apply[A, B](implicit ev: Plus[A, B]): Aux[A, B, ev.Out] = ev

  implicit def materializePlus[C, A <: C, B <: C](
      implicit nc: Numeric[C]
  ): Plus[A, B] = macro PlusMacro.materialize[C, A, B]

  @bundle
  final class PlusMacro(val c: whitebox.Context) extends MacroUtils {
    def materialize[C, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nc: c.Expr[Numeric[C]]
    ): c.Tree =
      materializeBinaryOp[Plus, A, B].apply(evalTyped(nc).plus)
  }
}
