package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import singleton.twoface.TwoFace

import scala.reflect.macros.whitebox

trait Checked1Param[Chk[_,_], Cond[_,_], Msg[_,_], Face, Param, ParamFace] extends Any with TwoFaceAny[Face] {
  def unsafeCheck(p : ParamFace)
  (implicit
   cond : TwoFace.Boolean.Shell2[Cond, Face, Face, ParamFace, ParamFace],
   msg : TwoFace.String.Shell2[Msg, Face, Face, ParamFace, ParamFace],
   req : TwoFace.Boolean.Shell2[RequireMsg, Cond[Face, ParamFace], std.Boolean, Msg[Face, ParamFace], std.String]
  ) : Chk[T, Param] = {
    req(cond(getValue, p).getValue, msg(getValue, p).getValue)
    this.asInstanceOf[Chk[T, Param]]
  }
}

object Checked1Param {
  trait Builder[Chk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace] {
    type Shell[T, Param] <: Checked1ParamShell[Chk, Face, T, Param]

    ////////////////////////////////////////////////////////////////////////////////////////
    // Manual invocations (usually used to for testing)
    ////////////////////////////////////////////////////////////////////////////////////////
    def apply[T, Param](implicit value : AcceptNonLiteral[Id[T]], param : AcceptNonLiteral[Id[Param]])
    : Chk[value.Out, param.Out] = macro Builder.Macro.fromOpApply[Chk[_,_], Cond[_,_], Msg[_,_]]

//    def apply[T <: Face, Param <: ParamFace](value : T)(param : Param)
//    : Chk[T, Param] = macro Builder.Macro.fromNumValue[Chk[_,_], Cond[_,_], Msg[_,_]]
//
//    def apply[T0 <: Face, Param <: ParamFace](value : TwoFaceAny[Face]{type T = T0})(param : TwoFaceAny[ParamFace]{type T = Param})
//    : Chk[T0, Param] = macro Builder.Macro.fromTF[Chk[_,_], Cond[_,_], Msg[_,_]]
    ////////////////////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////////////////////
    // Implicit Conversions
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[T, Param](implicit value : AcceptNonLiteral[Id[T]], param : AcceptNonLiteral[Id[Param]])
    : Chk[T, Param] = macro Builder.Macro.fromOpImpl[T, Param, Chk[_,_], Cond[_,_], Msg[_,_]]

    implicit def fromNum[T <: Face, Param <: ParamFace, Out <: T](value : T)(implicit param : AcceptNonLiteral[Id[Param]])
    : Chk[Out, Param] = macro Builder.Macro.fromNumValue[Chk[_,_], Cond[_,_], Msg[_,_]]

    implicit def fromTF[T0 <: Face, Param <: ParamFace, Out <: T0](value : TwoFaceAny[Face]{type T = T0})(implicit param : AcceptNonLiteral[Id[Param]])
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


trait Checked1ParamShell[Chk[_,_], Face, T, Param] {
  def apply(value : Face) : Chk[_,_]
}

object Checked1ParamShell {
  trait Builder[ChkShl[_,_], Chk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace] {
    type MsgHelper[T, Param] = ITE[IsNonLiteral[Msg[T, Param]], SomethingBadHappened, Msg[T, Param]]
    type CondHelper[T, Param] =
      RequireMsgSym[ITE[IsNonLiteral[Cond[T, Param]], True, Cond[T, Param]], MsgHelper[T, Param], ChkShl[_,_]]
    def create[T, Param] : ChkShl[T, Param]

    implicit def impl[T, Param]
    (implicit vc : CondHelper[T, Param]) :
    ChkShl[T, Param] = create[T, Param]
  }
}
