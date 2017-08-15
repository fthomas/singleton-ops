package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._

import scala.reflect.macros.whitebox

trait Checked1Param[Chk[_,_], Cond[_,_], Msg[_,_], Face, T, ParamFace, Param] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck(p : ParamFace)
    (implicit shl : CheckedShell2[Cond, Msg, Face, Face, ParamFace, ParamFace]) : Chk[T, Param] = {
    shl.unsafeCheck(getValue, p)
    this.asInstanceOf[Chk[T, Param]]
  }
}

object Checked1Param {
  trait Builder[Chk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace] {
    type Shell[T, Param] = CheckedShell2[Cond, Msg, T, Face, Param, ParamFace]

    ////////////////////////////////////////////////////////////////////////////////////////
    // Manual invocations (usually used to for testing)
    ////////////////////////////////////////////////////////////////////////////////////////
    def apply[T, Param](implicit value : AcceptNonLiteral[Id[T]], param : AcceptNonLiteral[Id[Param]])
    : Chk[value.Out, param.Out] = macro Builder.Macro.fromOpApply[Chk[_,_], Cond[_,_], Msg[_,_]]

//    def apply[T <: Face, Param <: ParamFace](value : T)(param : Param)
//    : Chk[T, Param] = macro Builder.Macro.fromNumValue[Chk[_,_], Cond[_,_], Msg[_,_]
//
//    def apply[T <: Face, Param <: ParamFace](value : TwoFaceAny[Face]{type T = T})(param : TwoFaceAny[ParamFace]{type T = Param})
//    : Chk[T, Param] = macro Builder.Macro.fromTF[Chk[_,_], Cond[_,_], Msg[_,_]]
    ////////////////////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////////////////////
    // Implicit Conversions
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[T, Param](implicit value : AcceptNonLiteral[Id[T]], param : AcceptNonLiteral[Id[Param]])
    : Chk[T, Param] = macro Builder.Macro.fromOpImpl[T, Param, Chk[_,_], Cond[_,_], Msg[_,_]]

    implicit def fromNum[T <: Face, Param <: ParamFace, Out <: T](value : T)(implicit param : AcceptNonLiteral[Id[Param]])
    : Chk[Out, Param] = macro Builder.Macro.fromNumValue[Chk[_,_], Cond[_,_], Msg[_,_]]

    implicit def fromTF[T <: Face, Param <: ParamFace, Out <: T](value : TwoFaceAny[Face, T])(implicit param : AcceptNonLiteral[Id[Param]])
    : Chk[Out, Param] = macro Builder.Macro.fromTF[Chk[_,_], Cond[_,_], Msg[_,_]]
    ////////////////////////////////////////////////////////////////////////////////////////
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromOpApply[Chk, Cond, Msg](value : c.Tree, param : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg].fromOpApply(value, param)

      def fromOpImpl[T, Param, Chk, Cond, Msg](value : c.Tree, param : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], p : c.WeakTypeTag[Param], chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg].fromOpImpl(value, c.weakTypeOf[T], param, c.weakTypeOf[Param])

      def fromNumValue[Chk, Cond, Msg](value : c.Tree)(param : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg].fromNumValue(value, param)

      def fromTF[Chk, Cond, Msg](value : c.Tree)(param : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked1ParamMaterializer[Chk, Cond, Msg].fromTF(value, param)
    }
  }
}

