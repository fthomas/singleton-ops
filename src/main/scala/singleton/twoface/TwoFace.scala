package singleton.twoface

import impl._

object TwoFace {
  type Char[T0] = TwoFaceAny.Char[T0]
  val Char = TwoFaceAny.Char

  type Int[T0] = TwoFaceAny.Int[T0]
  val Int = TwoFaceAny.Int

  type Long[T0] = TwoFaceAny.Long[T0]
  val Long = TwoFaceAny.Long

  type Float[T0] = TwoFaceAny.Float[T0]
  val Float = TwoFaceAny.Float

  type Double[T0] = TwoFaceAny.Double[T0]
  val Double = TwoFaceAny.Double

  type String[T0] = TwoFaceAny.String[T0]
  val String = TwoFaceAny.String

  type Boolean[T0] = TwoFaceAny.Boolean[T0]
  val Boolean = TwoFaceAny.Boolean
}
