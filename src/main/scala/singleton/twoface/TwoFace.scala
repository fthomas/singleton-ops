package singleton.twoface

import impl._

object TwoFace {
  type Char = TwoFaceAny.Char
  val Char = TwoFaceAny.Char

  type Int = TwoFaceAny.Int
  val Int = TwoFaceAny.Int

  type Long = TwoFaceAny.Long
  val Long = TwoFaceAny.Long

  type Float = TwoFaceAny.Float
  val Float = TwoFaceAny.Float

  type Double = TwoFaceAny.Double
  val Double = TwoFaceAny.Double

  type String = TwoFaceAny.String
  val String = TwoFaceAny.String

  type Boolean = TwoFaceAny.Boolean
  val Boolean = TwoFaceAny.Boolean
}