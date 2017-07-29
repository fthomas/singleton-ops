package singleton.twoface

import impl._

object TwoFace {
  type Char[T <: scala.Char] = TwoFaceAny.Char[T]
  val Char = TwoFaceAny.Char

  type Int[T <: scala.Int] = TwoFaceAny.Int[T]
  val Int = TwoFaceAny.Int

  type Long[T <: scala.Long] = TwoFaceAny.Long[T]
  val Long = TwoFaceAny.Long

  type Float[T <: scala.Float] = TwoFaceAny.Float[T]
  val Float = TwoFaceAny.Float

  type Double[T <: scala.Double] = TwoFaceAny.Double[T]
  val Double = TwoFaceAny.Double

  type String[T <: java.lang.String] = TwoFaceAny.String[T]
  val String = TwoFaceAny.String

  type Boolean[T <: scala.Boolean] = TwoFaceAny.Boolean[T]
  val Boolean = TwoFaceAny.Boolean
}