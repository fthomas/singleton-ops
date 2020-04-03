package singleton.twoface.impl

import singleton.ops._
import singleton.ops.impl._

import scala.reflect.macros.whitebox

trait Checked1ParamAny[Chk[Cond0[_,_], Msg0[_,_], T0, ParamFace0, Param0], Cond[_,_], Msg[_,_], Face, T, ParamFace, Param] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck(p : ParamFace)
    (implicit shl : CheckedShell2[Cond, Msg, NoSym, Face, Face, ParamFace, ParamFace]) : Chk[Cond, Msg, T, ParamFace, Param] = {
    shl.unsafeCheck(getValue, p)
    this.asInstanceOf[Chk[Cond, Msg, T, ParamFace, Param]]
  }
}

object Checked1ParamAny {
  trait LP[Chk[Cond0[_,_], Msg0[_,_], T0, ParamFace0, Param0], Face] {
    def create[Cond[_,_], Msg[_,_], T, ParamFace, Param](value : Face) : Chk[Cond, Msg, T, ParamFace, Param]

    //This is a hack to force Scalac to actually display the constructed error message in all cases
    //From some weird reason only direct macro calls displays the error in a case of an implicit conversion
    implicit def requireMsg[Cond[_,_], Msg[_,_], T >: Face, ParamFace, Param, Out <: T](value : T)
    : Chk[Cond, Msg, Out, ParamFace, Param] = macro Builder.Macro.requireMsg[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param]

    //from non-singleton values
    implicit def fromNumNonSing[Cond[_,_], Msg[_,_], ParamFace, Param](value : Face)(
      implicit req : RequireMsg[IsNonLiteral[GetArg0] || IsNonLiteral[Param] || Cond[GetArg0, Param], Msg[GetArg0, Param]],
    ) : Chk[Cond, Msg, Face, ParamFace, Param] = create[Cond, Msg, Face, ParamFace, Param](value)
  }
  trait Builder[Chk[Cond0[_,_], Msg0[_,_], T0, ParamFace0, Param0], Face] extends LP[Chk, Face]{
    trait Alias {
      type Cond[T, Param]
      type Msg[T, Param]
      type ParamFace
      final type Checked[T, Param] = Chk[Cond, Msg, T, ParamFace, Param]
      final type CheckedShell[T, Param] = CheckedShellSym[NoSym, T, Param]
      final type CheckedShellSym[Sym, T, Param] = CheckedShell2[Cond, Msg, Sym, T, Face, Param, ParamFace]
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    // Generic Implicit Conversions
    // Currently triggers good-code-red IntelliJ issue
    // https://youtrack.jetbrains.com/issue/SCL-13089
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[Cond[_,_], Msg[_,_], ParamFace, T, Param](
      implicit req : RequireMsg[IsNonLiteral[Param] || Cond[T, Param], Msg[T, Param]], value : AcceptNonLiteral[Id[T]]
    ) : Chk[Cond, Msg, T, ParamFace, Param] = create[Cond, Msg, T, ParamFace, Param](value.valueWide.asInstanceOf[Face])

    implicit def fromNumSing[Cond[_,_], Msg[_,_], T <: Face with Singleton, ParamFace, Param](value : T)(
      implicit req : RequireMsg[IsNonLiteral[Param] || Cond[T, Param], Msg[T, Param]]
    ) : Chk[Cond, Msg, T, ParamFace, Param] = create[Cond, Msg, T, ParamFace, Param](value)

    implicit def fromTF[Cond[_,_], Msg[_,_], T, ParamFace, Param](tfValue : TwoFaceAny[Face, T])(
      implicit req : RequireMsg[IsNonLiteral[T] || IsNonLiteral[Param] || Cond[T, Param], Msg[T, Param]], di : DummyImplicit
    ) : Chk[Cond, Msg, T, ParamFace, Param] = create[Cond, Msg, T, ParamFace, Param](tfValue.getValue)

    implicit def argCast[Cond[_,_], Msg[_,_], F, T, ParamFace, Param](c : Chk[Cond, Msg, F, ParamFace, Param])(
      implicit eq : OpContainer.Eq[F, T, Face]
    ) : Chk[Cond, Msg, T, ParamFace, Param] = c.asInstanceOf[Chk[Cond, Msg, T, ParamFace, Param]]
    ////////////////////////////////////////////////////////////////////////////////////////
  }

  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def requireMsg[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromNumValue(value)
    }
  }


  final class Char[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Char) extends
    Checked1ParamAny[Char, Cond, Msg, std.Char, T, ParamFace, Param] with TwoFaceAny.Char[T] {
    @inline def getValue : std.Char = value
  }
  object Char extends Builder[Char, std.Char] {
    override def create[Cond[_, _], Msg[_, _], T, ParamFace, Param](value: std.Char)
    : Char[Cond, Msg, T, ParamFace, Param] = new Char[Cond, Msg, T, ParamFace, Param](value)
  }
  
  final class Int[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Int) extends
    Checked1ParamAny[Int, Cond, Msg, std.Int, T, ParamFace, Param] with TwoFaceAny.Int[T] {
    @inline def getValue : std.Int = value
  }
  object Int extends Builder[Int, std.Int] {
    override def create[Cond[_, _], Msg[_, _], T, ParamFace, Param](value: std.Int)
    : Int[Cond, Msg, T, ParamFace, Param] = new Int[Cond, Msg, T, ParamFace, Param](value)
  }
  
  final class Long[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Long) extends
    Checked1ParamAny[Long, Cond, Msg, std.Long, T, ParamFace, Param] with TwoFaceAny.Long[T] {
    @inline def getValue : std.Long = value
  }
  object Long extends Builder[Long, std.Long] {
    override def create[Cond[_, _], Msg[_, _], T, ParamFace, Param](value: std.Long)
    : Long[Cond, Msg, T, ParamFace, Param] = new Long[Cond, Msg, T, ParamFace, Param](value)
  }
  
  final class Float[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Float) extends
    Checked1ParamAny[Float, Cond, Msg, std.Float, T, ParamFace, Param] with TwoFaceAny.Float[T] {
    @inline def getValue : std.Float = value
  }
  object Float extends Builder[Float, std.Float] {
    override def create[Cond[_, _], Msg[_, _], T, ParamFace, Param](value: std.Float)
    : Float[Cond, Msg, T, ParamFace, Param] = new Float[Cond, Msg, T, ParamFace, Param](value)
  }
  
  final class Double[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Double) extends
    Checked1ParamAny[Double, Cond, Msg, std.Double, T, ParamFace, Param] with TwoFaceAny.Double[T] {
    @inline def getValue : std.Double = value
  }
  object Double extends Builder[Double, std.Double] {
    override def create[Cond[_, _], Msg[_, _], T, ParamFace, Param](value: std.Double)
    : Double[Cond, Msg, T, ParamFace, Param] = new Double[Cond, Msg, T, ParamFace, Param](value)
  }
  
  final class String[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.String) extends
    Checked1ParamAny[String, Cond, Msg, std.String, T, ParamFace, Param] with TwoFaceAny.String[T] {
    @inline def getValue : std.String = value
  }
  object String extends Builder[String, std.String] {
    override def create[Cond[_, _], Msg[_, _], T, ParamFace, Param](value: std.String)
    : String[Cond, Msg, T, ParamFace, Param] = new String[Cond, Msg, T, ParamFace, Param](value)
  }
  
  final class Boolean[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Boolean) extends
    Checked1ParamAny[Boolean, Cond, Msg, std.Boolean, T, ParamFace, Param] with TwoFaceAny.Boolean[T] {
    @inline def getValue : std.Boolean = value
  }
  object Boolean extends Builder[Boolean, std.Boolean] {
    override def create[Cond[_, _], Msg[_, _], T, ParamFace, Param](value: std.Boolean)
    : Boolean[Cond, Msg, T, ParamFace, Param] = new Boolean[Cond, Msg, T, ParamFace, Param](value)
  }
}

