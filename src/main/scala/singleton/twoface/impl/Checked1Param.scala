package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

trait Checked1Param[Chk[_,_], Face, ParamFace, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck(p : ParamFace)
  (implicit rt : RunTime[T],
   rtc : Checked1Param.Runtime[Chk, Face, ParamFace]) : this.type = {
    if (rt) require(rtc.cond(getValue,p), rtc.msg(getValue,p))
    this
  }
}

object Checked1Param {
  trait Runtime[Chk[_,_], TFace, ParamFace] {
    def cond(t : TFace, p : ParamFace) : scala.Boolean
    def msg(t : TFace, p : ParamFace) : java.lang.String
  }

  trait Builder[Chk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace] {
    type Shell[T, Param] <: Checked1ParamShell[Chk, Face, T, Param]
    trait Runtime extends Checked1Param.Runtime[Chk, Face, ParamFace]
    type CondHelper[T, Param] = ITE[IsNotLiteral[Cond[T, Param]], true, Cond[T, Param]]
    type MsgHelper[T, Param] = ITE[IsNotLiteral[Msg[T, Param]], "Something bad happened", Msg[T, Param]]

    implicit def impl[T, Param]
      (implicit vc : CondHelper[T, Param], vm : MsgHelper[T, Param]) :
      Chk[T, Param] = macro Builder.Macro.impl[T, Param, Chk[T, Param]]

    implicit def safe[T <: Face with Singleton, Param]
      (value : T)(implicit vc : CondHelper[T, Param], vm : MsgHelper[T, Param]) :
      Chk[T, Param] = macro Builder.Macro.safe[T, Param, Chk[T, Param]]

    implicit def unsafe[Param](value : Face) :
      Chk[Face, Param] = macro Builder.Macro.unsafe[Face, Param, Chk[Face, Param]]

    implicit def safeTF[T <: Face with Singleton, Param]
      (value : TwoFaceAny[Face, T])(implicit vc : CondHelper[T, Param], vm : MsgHelper[T, Param]) :
      Chk[T, Param] = macro Builder.Macro.safeTF[T, Param, Chk[T, Param]]

    implicit def unsafeTF[Param](value : TwoFaceAny[Face, Face]) :
      Chk[Face, Param] = macro Builder.Macro.unsafeTF[Face, Param, Chk[Face, Param]]
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T, Param, Chk](vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].impl(1, vc, vm)

      def safe[T, Param, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].impl(1, vc, vm)

      def safeTF[T, Param, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].impl(1, vc, vm)

      def unsafe[T, Param, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].unsafe(1, value)

      def unsafeTF[T, Param, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].unsafeTF(1, value)
    }
  }
}


trait Checked1ParamShell[Chk[_,_], Face, T, Param] {
  def apply(value : Face) : Chk[_,_]
}

object Checked1ParamShell {
  trait Builder[ChkShl[_,_], Chk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace] {
    type MsgHelper[T, Param] = ITE[IsNotLiteral[Msg[T, Param]], "Something bad happened", Msg[T, Param]]
    type CondHelper[T, Param] =
      RequireMsgSym[ITE[IsNotLiteral[Cond[T, Param]], true, Cond[T, Param]], MsgHelper[T, Param], ChkShl[_,_]]
    def create[T, Param] : ChkShl[T, Param]

    implicit def impl[T, Param]
    (implicit vc : CondHelper[T, Param]) :
    ChkShl[T, Param] = create[T, Param]
  }
}
