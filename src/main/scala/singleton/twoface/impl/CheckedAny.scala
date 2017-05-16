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
    implicit def apply[T, Cond[_], Msg](tf : TwoFaceAny[Face, T])(
      implicit ct : CompileTime[Cond[T]] //eq : Require[ITE[IsNotLiteral[T], true, T==T2]],
    ) : CHK[T, Cond, Msg] = create[T, Cond, Msg](tf.getValue)
    implicit def apply[T, Cond[_], Msg, Out](value : T) : CHK[Out, Cond, Msg] = macro Builder.Macro.impl[T, Cond, Msg, Out]
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T, Cond[_], Msg, Out](value : c.Expr[T])(
        implicit cond : c.WeakTypeTag[Cond[_]], msg : c.WeakTypeTag[Msg]
      ): c.Tree = materializeOpVal[T, Cond[_], Msg, Out].usingFuncName(value)
    }
  }
}
