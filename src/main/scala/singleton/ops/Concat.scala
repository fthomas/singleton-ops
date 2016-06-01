package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.macros.MacroUtils

trait Concat[A, B] {
  type Out
}

object Concat {
  type Aux[A, B, Out0] = Concat[A, B] { type Out = Out0 }

  def apply[A, B](implicit ev: Concat[A, B]): Aux[A, B, ev.Out] = ev

  implicit def materializeConcat[A <: String, B <: String]: Concat[A, B] = macro ConcatMacro
    .materialize[A, B]

  @bundle
  final class ConcatMacro(val c: whitebox.Context) extends MacroUtils {
    def materialize[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Tree =
      materializeBinaryOp[Concat, A, B].apply[String](_ + _)
  }
}
