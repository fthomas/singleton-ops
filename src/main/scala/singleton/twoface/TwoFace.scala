package singleton.twoface

import impl._

object TwoFace {
  type Char[T] = TwoFaceAny.Char.Aux[T]
  val Char = TwoFaceAny.Char

  type Int[T] = TwoFaceAny.Int.Aux[T]
  val Int = TwoFaceAny.Int

  type Long[T] = TwoFaceAny.Long.Aux[T]
  val Long = TwoFaceAny.Long

  type Float[T] = TwoFaceAny.Float.Aux[T]
  val Float = TwoFaceAny.Float

  type Double[T] = TwoFaceAny.Double.Aux[T]
  val Double = TwoFaceAny.Double

  type String[T] = TwoFaceAny.String.Aux[T]
  val String = TwoFaceAny.String

  type Boolean[T] = TwoFaceAny.Boolean.Aux[T]
  val Boolean = TwoFaceAny.Boolean
}