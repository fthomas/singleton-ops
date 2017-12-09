package singleton.twoface
import impl._
import singleton.ops.impl.std

object Checked {
  type F1[_]
  type F2[_,_]
  type Shell1[Cond[_], Msg[_], Arg1, Arg1Wide] =
    impl.CheckedShell1[Cond, Msg, impl.CheckedShell1[F1,F1,_,_,_], Arg1, Arg1Wide]
  type Shell1Sym[Cond[_], Msg[_], Sym, Arg1, Arg1Wide] =
    impl.CheckedShell1[Cond, Msg, Sym, Arg1, Arg1Wide]
  type Shell2[Cond[_,_], Msg[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] =
    impl.CheckedShell2[Cond, Msg, impl.CheckedShell2[F2,F2,_,_,_,_,_], Arg1, Arg1Wide, Arg2, Arg2Wide]
  type Shell2Sym[Cond[_,_], Msg[_,_], Sym, Arg1, Arg1Wide, Arg2, Arg2Wide] =
    impl.CheckedShell2[Cond, Msg, Sym, Arg1, Arg1Wide, Arg2, Arg2Wide]
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
  object Char {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Char, T, ParamFace, Param] with TwoFaceAny.Char[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Char, ParamFace]
  }
  object Int {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Int, T, ParamFace, Param] with TwoFaceAny.Int[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Int, ParamFace]
  }
  object Long {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Long, T, ParamFace, Param] with TwoFaceAny.Long[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Long, ParamFace]
  }
  object Float {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Float, T, ParamFace, Param] with TwoFaceAny.Float[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Float, ParamFace]
  }
  object Double {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Double, T, ParamFace, Param] with TwoFaceAny.Double[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Double, ParamFace]
  }
  object String {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.String, T, ParamFace, Param] with TwoFaceAny.String[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.String, ParamFace]
  }
  object Boolean {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Boolean, T, ParamFace, Param] with TwoFaceAny.Boolean[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Boolean, ParamFace]
  }
}