package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox

trait Plus[A, B] {
  type Out
}

object Plus {
  type Aux[A, B, Out0] = Plus[A, B] { type Out = Out0 }

  def apply[A, B](implicit p: Plus[A, B]): Aux[A, B, p.Out] = p

  implicit def materializePlus[C, A <: C, B <: C](
      implicit nc: Numeric[C]
  ): Plus[A, B] = macro PlusMacro.materialize[C, A, B]

  @bundle
  class PlusMacro(override val c: whitebox.Context) extends MacroUtils(c) {
    import c.universe._

    def materialize[C, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nc: c.Expr[Numeric[C]]
    ): Tree = {
      val numeric = evalTyped(nc)
      materializeHelper(numeric.plus)(weakTypeOf[A], weakTypeOf[B]) {
        mkBinaryTypeClass(symbolOf[Plus[_, _]])
      }
    }
  }
}
