package singleton.twoface

import impl._
import singleton.ops.impl.std

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

object Checked0Param {
  //CC: traits used for the checked class
  //CO: traits used for the checked companion object
  object Char {
    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Char, T] with TwoFaceAny.Char[T]
    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Char]
  }
  object Int {
    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Int, T] with TwoFaceAny.Int[T]
    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Int]
  }
  object Long {
    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Long, T] with TwoFaceAny.Long[T]
    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Long]
  }
  object Float {
    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Float, T] with TwoFaceAny.Float[T]
    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Float]
  }
  object Double {
    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Double, T] with TwoFaceAny.Double[T]
    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Double]
  }
  object String {
    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.String, T] with TwoFaceAny.String[T]
    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.String]
  }
  object Boolean {
    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Boolean, T] with TwoFaceAny.Boolean[T]
    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Boolean]
  }
}

object Checked1Param {
  //CC: traits used for the checked class
  //CO: traits used for the checked companion object
  object String {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.String, T, ParamFace, Param] with TwoFaceAny.String[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.String, ParamFace]
  }
}