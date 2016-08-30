package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Minus[T, A, B] extends Op {
  override type Out <: T
}

object Minus extends Op2CompanionGen[Minus] {
  implicit def materializeMinus[T, A <: T, B <: T](
      implicit nt: Numeric[T]
  ): Minus[T, A, B] = macro MinusMacro.materialize[T, A, B]

  @ bundle
  final class MinusMacro(val c: whitebox.Context) extends Macros {
    def materialize[T: c.WeakTypeTag, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp2Gen[Minus, T, A, B].usingFunction(evalTyped(nt).minus)
  }
}
