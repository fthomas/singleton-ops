package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._

import scala.reflect.macros.whitebox

trait Checked1ParamAny[Chk[Cond0[_,_], Msg0[_,_], T0, ParamFace0, Param0], Cond[_,_], Msg[_,_], Face, T, ParamFace, Param] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck(p : ParamFace)
    (implicit shl : CheckedShell2[Cond, Msg, Chk[Cond,Msg,_,_,_], Face, Face, ParamFace, ParamFace]) : Chk[Cond, Msg, T, ParamFace, Param] = {
    shl.unsafeCheck(getValue, p)
    this.asInstanceOf[Chk[Cond, Msg, T, ParamFace, Param]]
  }
}

object Checked1ParamAny {
  trait Builder[Chk[Cond0[_,_], Msg0[_,_], T0, ParamFace0, Param0], Face] {
    trait Alias {
      type Cond[T, Param]
      type Msg[T, Param]
      type ParamFace
      final type Checked[T, Param] = Chk[Cond, Msg, T, ParamFace, Param]
      final type CheckedShell[T, Param] = CheckedShellSym[CheckedShellSym[_,_,_], T, Param]
      final type CheckedShellSym[Sym, T, Param] = CheckedShell2[Cond, Msg, Sym, T, Face, Param, ParamFace]
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    // Generic Implicit Conversions
    // Currently triggers good-code-red IntelliJ issue
    // https://youtrack.jetbrains.com/issue/SCL-13089
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[Cond[_,_], Msg[_,_], ParamFace, T, Param](implicit value : AcceptNonLiteral[Id[T]])
    : Chk[Cond, Msg, T, ParamFace, Param] = macro Builder.Macro.fromOpImpl[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param]

    implicit def fromNum[Cond[_,_], Msg[_,_], T >: Face, ParamFace, Param, Out <: T](value : T)
    : Chk[Cond, Msg, Out, ParamFace, Param] = macro Builder.Macro.fromNumValue[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param]

    implicit def fromTF[Cond[_,_], Msg[_,_], T >: Face, ParamFace, Param, Out <: T](value : TwoFaceAny[Face, T])
    : Chk[Cond, Msg, Out, ParamFace, Param] = macro Builder.Macro.fromTF[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param]

    implicit def widen[Cond[_,_], Msg[_,_], T, ParamFace, Param](value : Chk[Cond, Msg, T, ParamFace, Param])
    : Chk[Cond, Msg, Face, ParamFace, Param] = macro Builder.Macro.widen[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], Face, ParamFace, Param]
    ////////////////////////////////////////////////////////////////////////////////////////
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromOpImpl[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromOpImpl(value)

      def fromNumValue[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromNumValue(value)

      def fromTF[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromTF(value)

      def widen[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].widen(value)
    }
  }


  final class Char[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Char) extends
    Checked1ParamAny[Char, Cond, Msg, std.Char, T, ParamFace, Param] with TwoFaceAny.Char[T] {
    @inline def getValue : std.Char = value
  }
  object Char extends Builder[String, std.String]
  
  final class Int[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Int) extends
    Checked1ParamAny[Int, Cond, Msg, std.Int, T, ParamFace, Param] with TwoFaceAny.Int[T] {
    @inline def getValue : std.Int = value
  }
  object Int extends Builder[Int, std.Int]
  
  final class Long[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Long) extends
    Checked1ParamAny[Long, Cond, Msg, std.Long, T, ParamFace, Param] with TwoFaceAny.Long[T] {
    @inline def getValue : std.Long = value
  }
  object Long extends Builder[Long, std.Long]
  
  final class Float[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Float) extends
    Checked1ParamAny[Float, Cond, Msg, std.Float, T, ParamFace, Param] with TwoFaceAny.Float[T] {
    @inline def getValue : std.Float = value
  }
  object Float extends Builder[Float, std.Float]
  
  final class Double[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Double) extends
    Checked1ParamAny[Double, Cond, Msg, std.Double, T, ParamFace, Param] with TwoFaceAny.Double[T] {
    @inline def getValue : std.Double = value
  }
  object Double extends Builder[Double, std.Double]
  
  final class String[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.String) extends
    Checked1ParamAny[String, Cond, Msg, std.String, T, ParamFace, Param] with TwoFaceAny.String[T] {
    @inline def getValue : std.String = value
  }
  object String extends Builder[String, std.String]
  
  final class Boolean[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.Boolean) extends
    Checked1ParamAny[Boolean, Cond, Msg, std.Boolean, T, ParamFace, Param] with TwoFaceAny.Boolean[T] {
    @inline def getValue : std.Boolean = value
  }
  object Boolean extends Builder[Boolean, std.Boolean]
}

