package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait ToDouble[A] extends Op {
  override type Out <: Double
}

object ToDouble extends Op1Companion[ToDouble] {

  implicit def materializeToDouble[T, A <: T](
      implicit nt: Numeric[T]
  ): ToDouble[A] = macro ToDoubleMacro.materialize[T, A]

  @bundle
  final class ToDoubleMacro(val c: whitebox.Context) extends Macros {
    def materialize[T, A: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]
    ): c.Tree =
      materializeOp1[ToDouble, A].usingFunction(evalTyped(nt).toDouble)
  }
}
