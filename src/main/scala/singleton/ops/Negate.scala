package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.macros.MacroUtils

trait Negate[A] extends Op

object Negate {
  type Aux[A, Out0] = Negate[A] { type Out = Out0 }

  def apply[A](implicit ev: Negate[A]): Aux[A, ev.Out] = ev

  implicit def materializeNegate[T, A <: T](
      implicit nt: Numeric[T]
  ): Negate[A] = macro NegateMacro.materialize[T, A]

  @bundle
  final class NegateMacro(val c: whitebox.Context) extends MacroUtils {
    def materialize[T, A: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeUnaryOp[Negate, A].apply(evalTyped(nt).negate)
  }
}
