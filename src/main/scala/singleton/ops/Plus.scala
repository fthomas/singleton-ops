package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.macros.MacroUtils

trait Plus[A, B] extends Op

object Plus extends Op2Companion[Plus] {

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
