package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

trait Checked0Param[Cond[_], Msg[_], Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck()
  (implicit rt : RunTime[T],
   rtc : Checked0Param.Runtime[Cond, Msg, Face]) : this.type = {
    if (rt) require(rtc.cond(getValue), rtc.msg(getValue))
    this
  }
}

object Checked0Param {
  trait Runtime[Cond[_], Msg[_], TFace] {
    def cond(t : TFace) : scala.Boolean
    def msg(t : TFace) : java.lang.String
  }

  trait Builder[Chk[_], Cond[_], Msg[_], Face] {
    trait Runtime extends Checked0Param.Runtime[Cond, Msg, Face]
    type CondHelper[T] = ITE[IsNotLiteral[Cond[T]], true, Cond[T]]
    type MsgHelper[T] = ITE[IsNotLiteral[Msg[T]], "Something bad happened", Msg[T]]

    implicit def impl[T]
    (implicit vc : CondHelper[T], vm : MsgHelper[T]) :
    Chk[T] = macro Builder.Macro.impl[T, Chk[T]]

    implicit def safe[T <: Face with Singleton]
    (value : T)(implicit vc : CondHelper[T], vm : MsgHelper[T]) :
    Chk[T] = macro Builder.Macro.safe[T, Chk[T]]

    implicit def unsafe[Param](value : Face) :
    Chk[Face] = macro Builder.Macro.unsafe[Face, Chk[Face]]

    implicit def safeTF[T <: Face with Singleton]
    (value : TwoFaceAny[Face, T])(implicit vc : CondHelper[T], vm : MsgHelper[T]) :
    Chk[T] = macro Builder.Macro.safeTF[T, Chk[T]]

    implicit def unsafeTF[Param](value : TwoFaceAny[Face, Face]) :
    Chk[Face] = macro Builder.Macro.unsafeTF[Face, Chk[Face]]
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T, Chk](vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].impl(0, vc, vm)

      def safe[T, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].impl(0, vc, vm)

      def safeTF[T, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].impl(0, vc, vm)

      def unsafe[T, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].unsafe(0, value)

      def unsafeTF[T, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].unsafeTF(0, value)
    }
  }
}
