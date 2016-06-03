package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.macros.MacroUtils

trait Plus[A, B] extends Op

object Plus {
  type Aux[A, B, Out0] = Plus[A, B] { type Out = Out0 }

  def apply[A, B](implicit ev: Plus[A, B]): Aux[A, B, ev.Out] = ev

  implicit def materializePlus[T, A <: T, B <: T](
      implicit nt: Numeric[T]
  ): Plus[A, B] = macro PlusMacro.materialize[T, A, B]

  @bundle
  final class PlusMacro(val c: whitebox.Context) extends MacroUtils {
    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeBinaryOp[Plus, A, B].apply(evalTyped(nt).plus)
  }
}
