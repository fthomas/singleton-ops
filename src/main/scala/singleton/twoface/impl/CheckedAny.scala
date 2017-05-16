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
    implicit def apply[T, Cond[_], Msg, T2](tf : TwoFaceAny[Face, T2])(
      implicit eq : Require[ITE[IsNotLiteral[T], true, T==T2]], ct : CompileTime[Cond[T]]
    ) : CHK[T, Cond, Msg] = create[T, Cond, Msg](tf.getValue)
    implicit def apply[T, Cond[_], Msg](t : T) : CHK[T, Cond, Msg] = macro Builder.Macro.impl[T, Cond, Msg]
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T, Cond[_],Msg](t : c.Expr[T])(
        implicit cond : c.WeakTypeTag[Cond[_]], msg : c.WeakTypeTag[Msg]
      ): c.Tree = materializeOpVal[T, Cond[_], Msg].usingFuncName(t)
    }
  }
}
