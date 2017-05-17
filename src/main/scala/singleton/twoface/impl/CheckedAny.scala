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
//    implicit def apply[T](tf : TwoFaceAny[Face, T])(
//      implicit ct : CompileTime[Cond[T]] //eq : Require[ITE[IsNotLiteral[T], true, T==T2]],
//    ) : CHK[T] = create[T](tf.getValue)
    implicit def apply[T <: Int with Singleton](value : T) : CHK[T] = macro Builder.Macro.impl[T]
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T](value : c.Expr[T]): c.Tree = materializeOpVal[T].usingFuncName(value)
    }
  }
}
