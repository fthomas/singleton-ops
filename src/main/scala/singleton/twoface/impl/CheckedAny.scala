package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

object CheckedAny {
  trait Builder[CHK[_], Cond[_], Face] {
    protected def create[T](value : Face) : CHK[T]
    implicit def apply[T](implicit v : Id[T], ct : CompileTime[Cond[T]]) :
      CHK[T] = create[T](v.value.asInstanceOf[Face])
    implicit def safe[T <: Face with Singleton](value : T) : CHK[T] = macro Builder.Macro.impl[T]
    implicit def unsafe(value : Face) : CHK[Face] = macro Builder.Macro.impl[Face]
    implicit def safeTF[T <: Face with Singleton](tf : TwoFaceAny[Face, T])(
      implicit chk : CHK[T]//ct : CompileTime[Cond[T]] //eq : Require[ITE[IsNotLiteral[T], true, T==T2]],
    ) : CHK[T] = chk//create[T](tf.getValue)
    implicit def unsafeTF(tf : TwoFaceAny[Face, Face]) : CHK[Face] = create[Face](tf.getValue)
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T](value : c.Expr[T]): c.Expr[T] = materializeOpVal[T].usingFuncName(value)
    }
  }
}
