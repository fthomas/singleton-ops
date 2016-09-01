package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait LessThan[A, B] extends Op {
  override type Out <: Boolean
}

object LessThan extends Op2Companion[LessThan] {

  implicit def materializeLessThan[T, A <: T, B <: T](
      implicit ot: Ordering[T]
  ): LessThan[A, B] = macro LessThanMacro.materialize[T, A, B]

  @ bundle
  final class LessThanMacro(val c: whitebox.Context) extends Macros {
    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
        ot: c.Expr[Ordering[T]]
    ): c.Tree =
      materializeOp2[LessThan, A, B].usingPredicate(evalTyped(ot).lt)
  }
}
