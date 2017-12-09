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

  ///////////////////////////////////////////////
  // Template to define a Checked.Char type
  ///////////////////////////////////////////////
  //object /* Name this object */ {
  //  type Cond[T, P] = /* Your condition here */
  //  type Msg[T, P] = /* Your message here */
  //  type ParamFace = /* The parameter base type (e.g., Int) */
  //  final class Checked[T, Param](val value : Char) extends AnyVal with Checked1Param.Char.CC[Checked, Cond, Msg, T, ParamFace, Param] {
  //    @inline def getValue : Char = value
  //  }
  //  object Checked extends Checked1Param.Char.CO[Checked, Cond, Msg, ParamFace]
  //  object WorkAround extends impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
  //}
  ///////////////////////////////////////////////
  object Char {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Char, T, ParamFace, Param] with TwoFaceAny.Char[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Char, ParamFace]
  }

  ///////////////////////////////////////////////
  // Template to define a Checked.Int type
  ///////////////////////////////////////////////
  //object /* Name this object */ {
  //  type Cond[T, P] = /* Your condition here */
  //  type Msg[T, P] = /* Your message here */
  //  type ParamFace = /* The parameter base type (e.g., Int) */
  //  final class Checked[T, Param](val value : Int) extends AnyVal with Checked1Param.Int.CC[Checked, Cond, Msg, T, ParamFace, Param] {
  //    @inline def getValue : Int = value
  //  }
  //  object Checked extends Checked1Param.Int.CO[Checked, Cond, Msg, ParamFace]
  //  object WorkAround extends impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
  //}
  ///////////////////////////////////////////////
  object Int {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Int, T, ParamFace, Param] with TwoFaceAny.Int[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Int, ParamFace]
  }

  ///////////////////////////////////////////////
  // Template to define a Checked.Long type
  ///////////////////////////////////////////////
  //object /* Name this object */ {
  //  type Cond[T, P] = /* Your condition here */
  //  type Msg[T, P] = /* Your message here */
  //  type ParamFace = /* The parameter base type (e.g., Int) */
  //  final class Checked[T, Param](val value : Long) extends AnyVal with Checked1Param.Long.CC[Checked, Cond, Msg, T, ParamFace, Param] {
  //    @inline def getValue : Long = value
  //  }
  //  object Checked extends Checked1Param.Long.CO[Checked, Cond, Msg, ParamFace]
  //  object WorkAround extends impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
  //}
  ///////////////////////////////////////////////
  object Long {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Long, T, ParamFace, Param] with TwoFaceAny.Long[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Long, ParamFace]
  }

  ///////////////////////////////////////////////
  // Template to define a Checked.Float type
  ///////////////////////////////////////////////
  //object /* Name this object */ {
  //  type Cond[T, P] = /* Your condition here */
  //  type Msg[T, P] = /* Your message here */
  //  type ParamFace = /* The parameter base type (e.g., Int) */
  //  final class Checked[T, Param](val value : Float) extends AnyVal with Checked1Param.Float.CC[Checked, Cond, Msg, T, ParamFace, Param] {
  //    @inline def getValue : Float = value
  //  }
  //  object Checked extends Checked1Param.Float.CO[Checked, Cond, Msg, ParamFace]
  //  object WorkAround extends impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
  //}
  ///////////////////////////////////////////////
  object Float {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Float, T, ParamFace, Param] with TwoFaceAny.Float[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Float, ParamFace]
  }

  ///////////////////////////////////////////////
  // Template to define a Checked.Double type
  ///////////////////////////////////////////////
  //object /* Name this object */ {
  //  type Cond[T, P] = /* Your condition here */
  //  type Msg[T, P] = /* Your message here */
  //  type ParamFace = /* The parameter base type (e.g., Int) */
  //  final class Checked[T, Param](val value : Double) extends AnyVal with Checked1Param.Double.CC[Checked, Cond, Msg, T, ParamFace, Param] {
  //    @inline def getValue : Double = value
  //  }
  //  object Checked extends Checked1Param.Double.CO[Checked, Cond, Msg, ParamFace]
  //  object WorkAround extends impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
  //}
  ///////////////////////////////////////////////
  object Double {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Double, T, ParamFace, Param] with TwoFaceAny.Double[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Double, ParamFace]
  }

  ///////////////////////////////////////////////
  // Template to define a Checked.String type
  ///////////////////////////////////////////////
  //object /* Name this object */ {
  //  type Cond[T, P] = /* Your condition here */
  //  type Msg[T, P] = /* Your message here */
  //  type ParamFace = /* The parameter base type (e.g., Int) */
  //  final class Checked[T, Param](val value : String) extends AnyVal with Checked1Param.String.CC[Checked, Cond, Msg, T, ParamFace, Param] {
  //    @inline def getValue : String = value
  //  }
  //  object Checked extends Checked1Param.String.CO[Checked, Cond, Msg, ParamFace]
  //  object WorkAround extends impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
  //}
  ///////////////////////////////////////////////
  object String {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.String, T, ParamFace, Param] with TwoFaceAny.String[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.String, ParamFace]
  }

  ///////////////////////////////////////////////
  // Template to define a Checked.Boolean type
  ///////////////////////////////////////////////
  //object /* Name this object */ {
  //  type Cond[T, P] = /* Your condition here */
  //  type Msg[T, P] = /* Your message here */
  //  type ParamFace = /* The parameter base type (e.g., Int) */
  //  final class Checked[T, Param](val value : Boolean) extends AnyVal with Checked1Param.Boolean.CC[Checked, Cond, Msg, T, ParamFace, Param] {
  //    @inline def getValue : Boolean = value
  //  }
  //  object Checked extends Checked1Param.Boolean.CO[Checked, Cond, Msg, ParamFace]
  //  object WorkAround extends impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
  //}
  ///////////////////////////////////////////////
  object Boolean {
    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
      Checked1ParamAny[Chk, Cond, Msg, std.Boolean, T, ParamFace, Param] with TwoFaceAny.Boolean[T]
    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Boolean, ParamFace]
  }
}