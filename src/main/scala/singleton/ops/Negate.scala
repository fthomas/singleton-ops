package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Negate[T, A] extends Op {
  override type Out <: T
}

object Negate extends Op1CompanionGen[Negate] {

  implicit def materializeNegate[T, A <: T](
    implicit nt: Numeric[T]
  ): Negate[T, A] = macro NegateMacro.materialize[T, A]

  @bundle
  final class NegateMacro(val c: whitebox.Context) extends Macros {
    def materialize[T: c.WeakTypeTag, A: c.WeakTypeTag](
      nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp1Gen[Negate, T, A].usingFunction(evalTyped(nt).negate)
  }
}
