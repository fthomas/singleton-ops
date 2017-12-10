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
      type Checked[T, Param] = Chk[Cond, Msg, T, ParamFace, Param]
      type CheckedShell[T, Param] = CheckedShellSym[CheckedShellSym[_,_,_], T, Param]
      type CheckedShellSym[Sym, T, Param] = CheckedShell2[Cond, Msg, Sym, T, Face, Param, ParamFace]
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    // Generic Implicit Conversions
    // Currently triggers good-code-red IntelliJ issue
    // https://youtrack.jetbrains.com/issue/SCL-13089
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[Cond[_,_], Msg[_,_], T, ParamFace, Param](implicit value : AcceptNonLiteral[Id[T]], param : AcceptNonLiteral[Id[Param]])
    : Chk[Cond, Msg, T, ParamFace, Param] = macro Builder.Macro.fromOpImpl[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param]

    implicit def fromNum[Cond[_,_], Msg[_,_], T >: Face, ParamFace, Param, Out <: T](value : T)(implicit param : AcceptNonLiteral[Id[Param]])
    : Chk[Cond, Msg, Out, ParamFace, Param] = macro Builder.Macro.fromNumValue[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param]

    implicit def fromTF[Cond[_,_], Msg[_,_], T >: Face, ParamFace, Param, Out <: T](value : TwoFaceAny[Face, T])(implicit param : AcceptNonLiteral[Id[Param]])
    : Chk[Cond, Msg, Out, ParamFace, Param] = macro Builder.Macro.fromTF[Chk[Cond,Msg,_,_,_], Cond[_,_], Msg[_,_], T, ParamFace, Param]
    ////////////////////////////////////////////////////////////////////////////////////////
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromOpApply[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree, param : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromOpApply(value, param)

      def fromOpImpl[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree, param : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromOpImpl(value, param)

      def fromNumValue[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree)(param : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromNumValue(value, param)

      def fromTF[Chk, Cond, Msg, T, ParamFace, Param](value : c.Tree)(param : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param].fromTF(value, param)
    }
  }


  final class String[Cond[_,_], Msg[_,_], T, ParamFace, Param](val value : std.String) extends
    Checked1ParamAny[String, Cond, Msg, std.String, T, ParamFace, Param] with TwoFaceAny.String[T] {
    @inline def getValue : std.String = value
  }
  object String extends Builder[String, std.String]

}

