package singleton.twoface
import impl._
//import singleton.ops.impl.std

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
  trait Char    extends Checked0ParamAny.Char.Alias
  trait Int     extends Checked0ParamAny.Int.Alias
  trait Long    extends Checked0ParamAny.Long.Alias
  trait Float   extends Checked0ParamAny.Float.Alias
  trait Double  extends Checked0ParamAny.Double.Alias
  trait String  extends Checked0ParamAny.String.Alias
  trait Boolean extends Checked0ParamAny.Boolean.Alias
}
//object Checked0Param {
//  //CC: traits used for the checked class
//  //CO: traits used for the checked companion object
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Char 0-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T] = /* Your condition here */
//  //  type Msg[T] = /* Your message here */
//  //  final class Checked[T](val value : Char) extends AnyVal with Checked0Param.Char.CC[Checked, Cond, Msg, T] {
//  //    @inline def getValue : Char = value
//  //  }
//  //  object Checked extends Checked0Param.Char.CO[Checked, Cond, Msg]
//  //  object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Char {
//    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Char, T] with TwoFaceAny.Char[T]
//    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Char]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Int 0-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T] = /* Your condition here */
//  //  type Msg[T] = /* Your message here */
//  //  final class Checked[T](val value : Int) extends AnyVal with Checked0Param.Int.CC[Checked, Cond, Msg, T] {
//  //    @inline def getValue : Int = value
//  //  }
//  //  object Checked extends Checked0Param.Int.CO[Checked, Cond, Msg]
//  //  object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Int {
//    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Int, T] with TwoFaceAny.Int[T]
//    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Int]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Long 0-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T] = /* Your condition here */
//  //  type Msg[T] = /* Your message here */
//  //  final class Checked[T](val value : Long) extends AnyVal with Checked0Param.Long.CC[Checked, Cond, Msg, T] {
//  //    @inline def getValue : Long = value
//  //  }
//  //  object Checked extends Checked0Param.Long.CO[Checked, Cond, Msg]
//  //  object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Long {
//    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Long, T] with TwoFaceAny.Long[T]
//    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Long]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Float 0-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T] = /* Your condition here */
//  //  type Msg[T] = /* Your message here */
//  //  final class Checked[T](val value : Float) extends AnyVal with Checked0Param.Float.CC[Checked, Cond, Msg, T] {
//  //    @inline def getValue : Float = value
//  //  }
//  //  object Checked extends Checked0Param.Float.CO[Checked, Cond, Msg]
//  //  object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Float {
//    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Float, T] with TwoFaceAny.Float[T]
//    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Float]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Double 0-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T] = /* Your condition here */
//  //  type Msg[T] = /* Your message here */
//  //  final class Checked[T](val value : Double) extends AnyVal with Checked0Param.Double.CC[Checked, Cond, Msg, T] {
//  //    @inline def getValue : Double = value
//  //  }
//  //  object Checked extends Checked0Param.Double.CO[Checked, Cond, Msg]
//  //  object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Double {
//    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Double, T] with TwoFaceAny.Double[T]
//    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Double]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.String 0-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T] = /* Your condition here */
//  //  type Msg[T] = /* Your message here */
//  //  final class Checked[T](val value : String) extends AnyVal with Checked0Param.String.CC[Checked, Cond, Msg, T] {
//  //    @inline def getValue : String = value
//  //  }
//  //  object Checked extends Checked0Param.String.CO[Checked, Cond, Msg]
//  //  object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object String {
//    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.String, T] with TwoFaceAny.String[T]
//    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.String]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Boolean 0-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T] = /* Your condition here */
//  //  type Msg[T] = /* Your message here */
//  //  final class Checked[T](val value : Boolean) extends AnyVal with Checked0Param.Boolean.CC[Checked, Cond, Msg, T] {
//  //    @inline def getValue : Boolean = value
//  //  }
//  //  object Checked extends Checked0Param.Boolean.CO[Checked, Cond, Msg]
//  //  object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Boolean {
//    trait CC[Chk[_], Cond[_], Msg[_], T] extends Any with Checked0ParamAny[Chk, Cond, Msg, std.Boolean, T] with TwoFaceAny.Boolean[T]
//    trait CO[Chk[_], Cond[_], Msg[_]]    extends Checked0ParamAny.Builder[Chk, Cond, Msg, std.Boolean]
//  }
//}
//
//object Checked1Param {
//  //CC: traits used for the checked class
//  //CO: traits used for the checked companion object
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Char 1-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T, P] = /* Your condition here */
//  //  type Msg[T, P] = /* Your message here */
//  //  type ParamFace = /* The parameter base type (e.g., Int) */
//  //  final class Checked[T, Param](val value : Char) extends AnyVal with Checked1Param.Char.CC[Checked, Cond, Msg, T, ParamFace, Param] {
//  //    @inline def getValue : Char = value
//  //  }
//  //  object Checked extends Checked1Param.Char.CO[Checked, Cond, Msg, ParamFace]
//  //  object WorkAround extends singleton.twoface.impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Char {
//    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
//      Checked1ParamAny[Chk, Cond, Msg, std.Char, T, ParamFace, Param] with TwoFaceAny.Char[T]
//    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Char, ParamFace]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Int 1-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T, P] = /* Your condition here */
//  //  type Msg[T, P] = /* Your message here */
//  //  type ParamFace = /* The parameter base type (e.g., Int) */
//  //  final class Checked[T, Param](val value : Int) extends AnyVal with Checked1Param.Int.CC[Checked, Cond, Msg, T, ParamFace, Param] {
//  //    @inline def getValue : Int = value
//  //  }
//  //  object Checked extends Checked1Param.Int.CO[Checked, Cond, Msg, ParamFace]
//  //  object WorkAround extends singleton.twoface.impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Int {
//    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
//      Checked1ParamAny[Chk, Cond, Msg, std.Int, T, ParamFace, Param] with TwoFaceAny.Int[T]
//    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Int, ParamFace]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Long 1-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T, P] = /* Your condition here */
//  //  type Msg[T, P] = /* Your message here */
//  //  type ParamFace = /* The parameter base type (e.g., Int) */
//  //  final class Checked[T, Param](val value : Long) extends AnyVal with Checked1Param.Long.CC[Checked, Cond, Msg, T, ParamFace, Param] {
//  //    @inline def getValue : Long = value
//  //  }
//  //  object Checked extends Checked1Param.Long.CO[Checked, Cond, Msg, ParamFace]
//  //  object WorkAround extends singleton.twoface.impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Long {
//    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
//      Checked1ParamAny[Chk, Cond, Msg, std.Long, T, ParamFace, Param] with TwoFaceAny.Long[T]
//    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Long, ParamFace]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Float 1-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T, P] = /* Your condition here */
//  //  type Msg[T, P] = /* Your message here */
//  //  type ParamFace = /* The parameter base type (e.g., Int) */
//  //  final class Checked[T, Param](val value : Float) extends AnyVal with Checked1Param.Float.CC[Checked, Cond, Msg, T, ParamFace, Param] {
//  //    @inline def getValue : Float = value
//  //  }
//  //  object Checked extends Checked1Param.Float.CO[Checked, Cond, Msg, ParamFace]
//  //  object WorkAround extends singleton.twoface.impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Float {
//    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
//      Checked1ParamAny[Chk, Cond, Msg, std.Float, T, ParamFace, Param] with TwoFaceAny.Float[T]
//    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Float, ParamFace]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Double 1-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T, P] = /* Your condition here */
//  //  type Msg[T, P] = /* Your message here */
//  //  type ParamFace = /* The parameter base type (e.g., Int) */
//  //  final class Checked[T, Param](val value : Double) extends AnyVal with Checked1Param.Double.CC[Checked, Cond, Msg, T, ParamFace, Param] {
//  //    @inline def getValue : Double = value
//  //  }
//  //  object Checked extends Checked1Param.Double.CO[Checked, Cond, Msg, ParamFace]
//  //  object WorkAround extends singleton.twoface.impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Double {
//    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
//      Checked1ParamAny[Chk, Cond, Msg, std.Double, T, ParamFace, Param] with TwoFaceAny.Double[T]
//    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Double, ParamFace]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.String 1-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T, P] = /* Your condition here */
//  //  type Msg[T, P] = /* Your message here */
//  //  type ParamFace = /* The parameter base type (e.g., Int) */
//  //  final class Checked[T, Param](val value : String) extends AnyVal with Checked1Param.String.CC[Checked, Cond, Msg, T, ParamFace, Param] {
//  //    @inline def getValue : String = value
//  //  }
//  //  object Checked extends Checked1Param.String.CO[Checked, Cond, Msg, ParamFace]
//  //  object WorkAround extends singleton.twoface.impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object String {
//    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
//      Checked1ParamAny[Chk, Cond, Msg, std.String, T, ParamFace, Param] with TwoFaceAny.String[T]
//    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.String, ParamFace]
//  }
//
//  /////////////////////////////////////////////////////
//  // Template to define a Checked.Boolean 1-Params type
//  /////////////////////////////////////////////////////
//  //object /* Name this object */ {
//  //  type Cond[T, P] = /* Your condition here */
//  //  type Msg[T, P] = /* Your message here */
//  //  type ParamFace = /* The parameter base type (e.g., Int) */
//  //  final class Checked[T, Param](val value : Boolean) extends AnyVal with Checked1Param.Boolean.CC[Checked, Cond, Msg, T, ParamFace, Param] {
//  //    @inline def getValue : Boolean = value
//  //  }
//  //  object Checked extends Checked1Param.Boolean.CO[Checked, Cond, Msg, ParamFace]
//  //  object WorkAround extends singleton.twoface.impl.Checked1ParamAny.Builder[Nothing, Nothing, Nothing, Nothing, Nothing]
//  //}
//  /////////////////////////////////////////////////////
//  object Boolean {
//    trait CC[Chk[_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param] extends Any with
//      Checked1ParamAny[Chk, Cond, Msg, std.Boolean, T, ParamFace, Param] with TwoFaceAny.Boolean[T]
//    trait CO[Chk[_,_], Cond[_,_], Msg[_,_], ParamFace] extends Checked1ParamAny.Builder[Chk, Cond, Msg, std.Boolean, ParamFace]
//  }
//}