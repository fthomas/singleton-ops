package singleton.ops.impl

import macrocompat.bundle
import shapeless.Nat

import scala.reflect.macros.whitebox

/********************************************************************************************************
  * Single argument type function macro
  *******************************************************************************************************/
trait Op1Macro[N <: String with Singleton, T1, S1 <: T1 with Singleton]
    extends SingletonTypeExpr

@ bundle
object Op1Macro {
  implicit def call[N <: String with Singleton, T1, S1 <: T1 with Singleton]: Op1Macro[
    N,
    T1,
    S1] = macro Macro.impl[N, T1, S1]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        N <: String with Singleton: c.WeakTypeTag,
        T1: c.WeakTypeTag,
        S1 <: T1 with Singleton: c.WeakTypeTag
    ]: c.Tree =
      materializeOp1Gen[Op1Macro[_, _, _], N, T1, S1].usingFuncName
  }
}

/*******************************************************************************************************/
/********************************************************************************************************
  * Two arguments type function macro
  *******************************************************************************************************/
trait Op2Macro[N <: String with Singleton,
               T1,
               S1 <: T1 with Singleton,
               T2,
               S2 <: T2 with Singleton]
    extends SingletonTypeExpr

@ bundle
object Op2Macro {
  implicit def call[N <: String with Singleton,
                    T1,
                    S1 <: T1 with Singleton,
                    T2,
                    S2 <: T2 with Singleton]: Op2Macro[N, T1, S1, T2, S2] =
    macro Macro.impl[N, T1, S1, T2, S2]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        N <: String with Singleton: c.WeakTypeTag,
        T1: c.WeakTypeTag,
        S1 <: T1 with Singleton: c.WeakTypeTag,
        T2: c.WeakTypeTag,
        S2 <: T2 with Singleton: c.WeakTypeTag
    ]: c.Tree =
      materializeOp2Gen[Op2Macro[_, _, _, _, _], N, T1, S1, T2, S2].usingFuncName
  }
}

/*******************************************************************************************************/
trait ToNatMacro[T1, S1 <: T1 with Singleton] {
  val value: Nat
}

@ bundle
object ToNatMacro {
  implicit def call[T1, S1 <: T1 with Singleton]: ToNatMacro[T1, S1] =
    macro Macro.impl[T1, S1]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        T1: c.WeakTypeTag,
        S1 <: T1 with Singleton: c.WeakTypeTag
    ]: c.Tree =
      materializeToNat[ToNatMacro[_, _], T1, S1].usingFuncName
  }
}

//
//trait FromNatMacro[N <: Nat] {
//  type Out <: Int with Singleton
//  val value : Out {}
//}
//
//@ bundle
//object FromNatMacro {
//  implicit def call[N <: Nat]: FromNatMacro[N] =
//  macro Macro.impl[N]
//
//  final class Macro(val c: whitebox.Context) extends GeneralMacros {
//    def impl[N <: Nat : c.WeakTypeTag]: c.Tree =
//      materializeFromNat[FromNatMacro[_], N].usingFuncName
//  }
//}