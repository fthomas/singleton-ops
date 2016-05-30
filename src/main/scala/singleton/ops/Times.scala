package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox

trait Times[A, B] {
  type Out
}

object Times {
  type Aux[A, B, Out0] = Times[A, B] { type Out = Out0 }

  def apply[A, B](implicit p: Times[A, B]): Aux[A, B, p.Out] = p

  implicit def materializeTimes[C, A <: C, B <: C](
      implicit nc: Numeric[C]
  ): Times[A, B] = macro TimesMacro.materialize[C, A, B]

  @bundle
  class TimesMacro(override val c: whitebox.Context) extends MacroUtils(c) {
    import c.universe._

    def materialize[C, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nc: c.Expr[Numeric[C]]
    ): Tree = {
      val numeric = c.eval(c.Expr[Numeric[C]](c.untypecheck(nc.tree)))
      materializeHelper(numeric.times)(weakTypeOf[A], weakTypeOf[B]) {
        (a, b, ab) =>
          q"new _root_.singleton.ops.Times[$a, $b] { type Out = $ab }"
      }
    }
  }
}
