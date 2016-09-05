package singleton.ops.impl

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Op {
  type Out
  val value: Out {}
}

trait SumMacro[T, A, B] extends Op

@bundle
object SumMacro {
  implicit def call[T, A, B](implicit nt: Numeric[T]): SumMacro[T, A, B] =
    macro Macro.impl[T, A, B]

  final class Macro(val c: whitebox.Context) extends Macros {
    def impl[
        T: c.WeakTypeTag,
        A: c.WeakTypeTag,
        B: c.WeakTypeTag
    ](nt: c.Expr[Numeric[T]]): c.Tree =
      materializeOp3[SumMacro[_, _, _], T, A, B]
        .usingFunction(evalTyped(nt).plus)
  }
}
