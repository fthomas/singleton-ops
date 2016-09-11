package singleton.ops

import macrocompat.bundle

import scala.reflect.macros.whitebox
import singleton.ops.impl.{GeneralMacros, _}

trait Negate[A] extends Op

object Negate extends Op1Companion[Negate] {

  implicit def materializeNegate[T, A <: T](
      implicit nt: Numeric[T]
  ): Negate[A] = macro NegateMacro.materialize[T, A]

  @bundle
  final class NegateMacro(val c: whitebox.Context) extends GeneralMacros {
    def materialize[T, A: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp1[Negate, A].usingFunction(evalTyped(nt).negate)
  }
}
