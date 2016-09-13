package singleton.ops.impl

import macrocompat.bundle

import scala.reflect.macros.whitebox

/********************************************************************************************************
  * Sum
  *******************************************************************************************************/
//trait SumMacro[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton]
//    extends Op2[T1, S1, T2, S2]
trait Op2Macro[N <: String with Singleton, T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton]
  extends SingletonTypeExpr

@bundle
object Op2Macro {
  implicit def call[N <: String with Singleton, T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton]:
  Op2Macro[N, T1, S1, T2, S2] = macro Macro.impl[N, T1, S1, T2, S2]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        N <: String with Singleton: c.WeakTypeTag,
        T1: c.WeakTypeTag,
        S1 <: T1 with Singleton: c.WeakTypeTag,
        T2: c.WeakTypeTag,
        S2 <: T2 with Singleton: c.WeakTypeTag
    ] : c.Tree =
      materializeOp2Gen[Op2Macro[_, _, _, _, _], N, T1, S1, T2, S2]
          .usingFuncName
  }
}

/*******************************************************************************************************/
/********************************************************************************************************
  * ToLong
  *******************************************************************************************************/
trait ToLongMacro[B, T1, S1 <: T1 with Singleton] extends Op1[B, T1, S1]

@bundle
object ToLongMacro {
  implicit def call[B, T1, S1 <: T1 with Singleton](
      implicit nt1: Numeric[T1]): ToLongMacro[B, T1, S1] =
    macro Macro.impl[B, T1, S1]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        B: c.WeakTypeTag,
        T1: c.WeakTypeTag,
        S1 <: T1 with Singleton: c.WeakTypeTag
    ](nt1: c.Expr[Numeric[T1]]): c.Tree =
      materializeOp1Gen[ToLongMacro[_, _, _], B, T1, S1]
        .usingFunction(evalTyped(nt1).toLong)
  }
}

/*******************************************************************************************************/
/********************************************************************************************************
  * ToInt
  *******************************************************************************************************/
trait ToIntMacro[B, T1, S1 <: T1 with Singleton] extends Op1[B, T1, S1]

@bundle
object ToIntMacro {
  implicit def call[B, T1, S1 <: T1 with Singleton](
      implicit nt1: Numeric[T1]): ToIntMacro[B, T1, S1] =
    macro Macro.impl[B, T1, S1]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        B: c.WeakTypeTag,
        T1: c.WeakTypeTag,
        S1 <: T1 with Singleton: c.WeakTypeTag
    ](nt1: c.Expr[Numeric[T1]]): c.Tree =
      materializeOp1Gen[ToIntMacro[_, _, _], B, T1, S1]
        .usingFunction(evalTyped(nt1).toInt)
  }
}
/*******************************************************************************************************/
