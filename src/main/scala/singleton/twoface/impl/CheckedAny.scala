package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

object CheckedAny {
  trait Builder[CHK[_,C[_],_], Face] {
    protected def create[T, Cond[_], Msg](value : Face) : CHK[T, Cond, Msg]
    implicit def apply[T, Cond[_], Msg](implicit v : Id[T], ct : CompileTime[Cond[T]]) :
      CHK[T, Cond, Msg] = create[T, Cond, Msg](v.value.asInstanceOf[Face])
    implicit def safe[T <: Face with Singleton, Cond[_], Msg](value : T) :
      CHK[T, Cond, Msg] = macro Builder.Macro.impl[T, Cond, Msg]
    implicit def unsafe[Cond[_], Msg](value : Face) :
      CHK[Face, Cond, Msg] = macro Builder.Macro.impl[Face, Cond, Msg]
    implicit def safeTF[T <: Face with Singleton, Cond[_], Msg]
      (tf : TwoFaceAny[Face, T])(implicit chk : CHK[T, Cond, Msg]) :
      CHK[T, Cond, Msg] = chk//create[T](tf.getValue)
    implicit def unsafeTF[Cond[_], Msg](tf : TwoFaceAny[Face, Face]) :
      CHK[Face, Cond, Msg] = create[Face, Cond, Msg](tf.getValue)
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T, Cond[_], Msg](value : c.Expr[T])(
        implicit cond : c.WeakTypeTag[Cond[_]], msg : c.WeakTypeTag[Msg]
      ): c.Tree = materializeOpVal[T, Cond[_], Msg].usingFuncName(value)
    }
  }
}
