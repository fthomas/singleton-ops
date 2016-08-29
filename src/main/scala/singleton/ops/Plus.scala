package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Plus[A, B] extends Op {
  //override type Out <: Int
}

trait PlusI[A, B] extends Plus[A, B] {
  override type Out <: Int
}

object Plus extends Op2Companion[Plus] {

  implicit def materializePlus[T, A <: T, B <: T](
      implicit nt: Numeric[T]
  ): Plus[A, B] = macro PlusMacro.materialize[T, A, B]

  @bundle
  final class PlusMacro(val c: whitebox.Context) extends Macros {
    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp2[Plus, A, B].usingFunction(evalTyped(nt).plus)
  }
}

object PlusI extends Op2Companion[PlusI] {

  implicit def materializePlusI[T, A <: T, B <: T](
                                                   implicit nt: Numeric[T]
                                                 ): PlusI[A, B] = macro PlusIMacro.materialize[T, A, B]

  @bundle
  final class PlusIMacro(val c: whitebox.Context) extends Macros {
    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
                                                            nt: c.Expr[Numeric[T]]
                                                          ): c.Tree =
      materializeOp2[PlusI, A, B].usingFunction(evalTyped(nt).plus)
  }
}