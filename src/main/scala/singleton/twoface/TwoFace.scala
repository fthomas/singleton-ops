package singleton.twoface

import impl._

object TwoFace {
  type Char[T] = TwoFaceAny.CharLike.Aux[T]
  val Char = TwoFaceAny.CharLike

  type Int[T0] = TwoFaceAny.IntLike{type T <: T0}
  val Int = TwoFaceAny.IntLike

  type Long[T] = TwoFaceAny.LongLike.Aux[T]
  val Long = TwoFaceAny.LongLike

  type Float[T] = TwoFaceAny.FloatLike.Aux[T]
  val Float = TwoFaceAny.FloatLike

  type Double[T] = TwoFaceAny.DoubleLike.Aux[T]
  val Double = TwoFaceAny.DoubleLike

  type String[T] = TwoFaceAny.StringLike.Aux[T]
  val String = TwoFaceAny.StringLike

  type Boolean[T] = TwoFaceAny.BooleanLike.Aux[T]
  val Boolean = TwoFaceAny.BooleanLike
}