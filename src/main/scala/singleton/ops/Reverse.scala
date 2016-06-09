package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Reverse[A] extends Op

object Reverse extends Op1Companion[Reverse] {

  implicit def materializeReverse[A <: String]: Reverse[A] = macro ReverseMacro
    .materialize[A]

  @bundle
  final class ReverseMacro(val c: whitebox.Context) extends Macros {
    def materialize[A: c.WeakTypeTag]: c.Tree =
      materializeUnaryOp[Reverse, A].apply[String](_.reverse)
  }
}
