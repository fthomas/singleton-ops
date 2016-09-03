package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Plus[T, A <: T with Singleton, B <: T with Singleton] extends Op2[T, A, B] {
//  override type Out <: T
}

object Plus extends Op2CompanionGen[Plus] {
  implicit def materializePlus[T, A <: T with Singleton, B <: T with Singleton](
      implicit nt: Numeric[T]
  ): Plus[T, A, B] = macro PlusMacro.materialize[T, A, B]

  @bundle
  final class PlusMacro(val c: whitebox.Context) extends Macros {
    def materialize[
    T : c.WeakTypeTag,
    A <: T with Singleton : c.WeakTypeTag,
    B <: T with Singleton : c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Expr[Plus[T, A, B]] =
      c.Expr(materializeOp2Gen[Plus, T, A, B].usingFunction(evalTyped(nt).plus))
  }
}
