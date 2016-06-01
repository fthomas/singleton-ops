package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.macros.MacroUtils

trait Reverse[A] {
  type Out
}

object Reverse {
  type Aux[A, Out0] = Reverse[A] { type Out = Out0 }

  def apply[A](implicit ev: Reverse[A]): Aux[A, ev.Out] = ev

  implicit def materializeReverse[A <: String]: Reverse[A] = macro ReverseMacro
    .materialize[A]

  @bundle
  final class ReverseMacro(val c: whitebox.Context) extends MacroUtils {
    def materialize[A: c.WeakTypeTag]: c.Tree =
      materializeUnaryOp[Reverse, A].apply[String](_.reverse)
  }
}
