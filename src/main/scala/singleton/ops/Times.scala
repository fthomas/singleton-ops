package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox

trait Times[A, B] {
  type Out
}

object Times {
  type Aux[A, B, Out0] = Times[A, B] { type Out = Out0 }

  def apply[A, B](implicit ev: Times[A, B]): Aux[A, B, ev.Out] = ev

  implicit def materializeTimes[C, A <: C, B <: C](
      implicit nc: Numeric[C]
  ): Times[A, B] = macro TimesMacro.materialize[C, A, B]

  @bundle
  final class TimesMacro(val c: whitebox.Context) extends MacroUtils {
    def materialize[C, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nc: c.Expr[Numeric[C]]
    ): c.Tree =
      materializeBinaryOp[Times, A, B].apply(evalTyped(nc).times)
  }
}
