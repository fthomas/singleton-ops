package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

trait CheckedAny[Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck[ParamFace, Cond[_,_], Msg[_,_]](p : Option[ParamFace] = None)
  (implicit rt : RunTime[T],
   rtc : CheckedAny.Runtime[Face, ParamFace, Cond, Msg]) : this.type = {
    if (rt) require(rtc.cond(getValue,p), rtc.msg(getValue,p))
    this
  }
}

object CheckedAny {
  trait Runtime[TFace, ParamFace, Cond[_,_], Msg[_,_]] {
    def cond(t : TFace, p : Option[ParamFace]) : scala.Boolean
    def msg(t : TFace, p : Option[ParamFace]) : java.lang.String
  }

  trait Builder[Chk[_,_],Cond[_,_],Msg[_,_],Face, ParamFace] {
    trait Runtime extends CheckedAny.Runtime[Face, ParamFace, Cond, Msg]
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
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].impl(vc, vm)

      def safe[T, Param, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].impl(vc, vm)

      def safeTF[T, Param, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].impl(vc, vm)

      def unsafe[T, Param, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].unsafe(value)

      def unsafeTF[T, Param, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Param, Chk].unsafeTF(value)
    }
  }
}
