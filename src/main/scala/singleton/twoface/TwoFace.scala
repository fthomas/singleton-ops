package singleton.twoface

import impl._

object TwoFace {
  type Char[T0] = TwoFaceAny._Char[T0]
  val Char = TwoFaceAny.CharLike

  type Int[T0] = TwoFaceAny._Int[T0]
  val Int = TwoFaceAny.IntLike

  type Long[T0] = TwoFaceAny._Long[T0]
  val Long = TwoFaceAny.LongLike

  type Float[T0] = TwoFaceAny._Float[T0]
  val Float = TwoFaceAny.FloatLike

  type Double[T0] = TwoFaceAny._Double[T0]
  val Double = TwoFaceAny.DoubleLike

  type String[T0] = TwoFaceAny._String[T0]
  val String = TwoFaceAny.StringLike

  type Boolean[T0] = TwoFaceAny._Boolean[T0]
  val Boolean = TwoFaceAny.BooleanLike
}