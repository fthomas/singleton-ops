package singleton.twoface

import impl._

object TwoFace {
  type Char[T0] = TwoFaceAny.CharLike{type T <: T0}
  val Char = TwoFaceAny.CharLike

  type Int[T0] = TwoFaceAny.IntLike{type T <: T0}
  val Int = TwoFaceAny.IntLike

  type Long[T0] = TwoFaceAny.LongLike{type T <: T0}
  val Long = TwoFaceAny.LongLike

  type Float[T0] = TwoFaceAny.FloatLike{type T <: T0}
  val Float = TwoFaceAny.FloatLike

  type Double[T0] = TwoFaceAny.DoubleLike{type T <: T0}
  val Double = TwoFaceAny.DoubleLike

  type String[T0] = TwoFaceAny.StringLike{type T <: T0}
  val String = TwoFaceAny.StringLike

  type Boolean[T0] = TwoFaceAny.BooleanLike{type T <: T0}
  val Boolean = TwoFaceAny.BooleanLike
}