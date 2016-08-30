package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Reverse[A] extends Op {
  override type Out <: String
}

object Reverse extends Op1Companion[Reverse] {

  implicit def materializeReverse[A <: String]: Reverse[A] =
    macro ReverseMacro.materialize[A]

  @ bundle
  final class ReverseMacro(val c: whitebox.Context) extends Macros {
    def materialize[A: c.WeakTypeTag]: c.Tree =
      materializeOp1[Reverse, A].usingFunction((_: String).reverse)
  }
}
