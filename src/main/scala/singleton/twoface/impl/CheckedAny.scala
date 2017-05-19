package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

object CheckedAny {
  trait Builder[Chk[_,C[_,_],_,_], Face] {
    protected def create[T, Cond[_,_], Param, Msg](value : Face) : Chk[T, Cond, Param, Msg]
    implicit def impl[T, Cond[_,_], Param, Msg](implicit v : Id[T], ct : CompileTime[Cond[T, Param]]) :
      Chk[T, Cond, Param, Msg] = create[T, Cond, Param, Msg](v.value.asInstanceOf[Face])
    implicit def safe[T <: Face with Singleton, Cond[_,_], Param, Msg](value : T) :
      Chk[T, Cond, Param, Msg] = macro Builder.Macro.safe[T, Cond, Param, Msg, Chk[T, Cond, Param, Msg]]
    implicit def unsafe[Cond[_,_], Param, Msg](value : Face) :
      Chk[Face, Cond, Param, Msg] = create[Face, Cond, Param, Msg](value)
    implicit def safeTF[T <: Face with Singleton, Cond[_,_], Param, Msg]
      (value : TwoFaceAny[Face, T]) :
      Chk[T, Cond, Param, Msg] = macro Builder.Macro.safeTF[value.type, T, Cond, Param, Msg, Chk[T, Cond, Param, Msg]]
    implicit def unsafeTF[Cond[_,_], Param, Msg](tf : TwoFaceAny[Face, Face]) :
      Chk[Face, Cond, Param, Msg] = create[Face, Cond, Param, Msg](tf.getValue)
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def safe[T, Cond[_,_], Param, Msg, Chk](value : c.Expr[T])(
        implicit
        cond : c.WeakTypeTag[Cond[_,_]], msg : c.WeakTypeTag[Msg], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedMaterializer[T, Cond[_,_], Param, Msg, Chk].safe(value)
      def safeTF[VT, T, Cond[_,_], Param, Msg, Chk](value : c.Expr[VT])(
        implicit
        t : c.WeakTypeTag[T], cond : c.WeakTypeTag[Cond[_,_]], msg : c.WeakTypeTag[Msg], param: c.WeakTypeTag[Param], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedMaterializer[T, Cond[_,_], Param, Msg, Chk].safeTF
    }
  }
}
