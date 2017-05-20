package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

object CheckedAny {
  trait Builder[Chk[_,C[_,_],_,M[_,_]], Face] {
    protected def create[T, Cond[_,_], Param, Msg[_,_]](value : Face) : Chk[T, Cond, Param, Msg]
    implicit def impl[T, Cond[_,_], Param, Msg[_,_]](implicit vc : SafeBoolean[ITE[IsNotLiteral[Cond[T, Param]], true, Cond[T, Param]]]) :
      Chk[T, Cond, Param, Msg] = macro Builder.Macro.impl[T, Cond, Param, Msg, Chk[T, Cond, Param, Msg]]//create[T, Cond, Param, Msg](v.value.asInstanceOf[Face])
    implicit def safe[T <: Face with Singleton, Cond[_,_], Param, Msg[_,_]](value : T) :
      Chk[T, Cond, Param, Msg] = macro Builder.Macro.safe[T, Cond, Param, Msg, Chk[T, Cond, Param, Msg]]
    implicit def unsafe[Cond[_,_], Param, Msg[_,_]](value : Face) :
      Chk[Face, Cond, Param, Msg] = create[Face, Cond, Param, Msg](value)
    implicit def safeTF[T <: Face with Singleton, Cond[_,_], Param, Msg[_,_]]
      (value : TwoFaceAny[Face, T]) :
      Chk[T, Cond, Param, Msg] = macro Builder.Macro.safeTF[value.type, T, Cond, Param, Msg, Chk[T, Cond, Param, Msg]]
    implicit def unsafeTF[Cond[_,_], Param, Msg[_,_]](tf : TwoFaceAny[Face, Face]) :
      Chk[Face, Cond, Param, Msg] = create[Face, Cond, Param, Msg](tf.getValue)
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T, Cond[_,_], Param, Msg[_,_], Chk](vc : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], cond : c.WeakTypeTag[Cond[_,_]], msg : c.WeakTypeTag[Msg[_,_]], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, Cond[_,_], Param, Msg[_,_], Chk].impl(vc)
      def safe[T, Cond[_,_], Param, Msg[_,_], Chk](value : c.Expr[T])(
        implicit
        t : c.WeakTypeTag[T], cond : c.WeakTypeTag[Cond[_,_]], msg : c.WeakTypeTag[Msg[_,_]], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedMaterializer[T, Cond[_,_], Param, Msg[_,_], Chk].safe
      def safeTF[VT, T, Cond[_,_], Param, Msg[_,_], Chk](value : c.Expr[VT])(
        implicit
        t : c.WeakTypeTag[T], cond : c.WeakTypeTag[Cond[_,_]], msg : c.WeakTypeTag[Msg[_,_]], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedMaterializer[T, Cond[_,_], Param, Msg[_,_], Chk].safe
    }
  }
}
