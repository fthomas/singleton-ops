package singleton.ops.impl

import macrocompat.bundle
import shapeless.Nat

import scala.reflect.macros.whitebox

/********************************************************************************************************
  * Single argument type function macro
  *******************************************************************************************************/
trait Op1Macro[N <: String with Singleton, S1] extends SingletonTypeExpr

@bundle
object Op1Macro {
  implicit def call[N <: String with Singleton, S1]: Op1Macro[N, S1] =
    macro Macro.impl[N, S1]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        N <: String with Singleton: c.WeakTypeTag,
        S1: c.WeakTypeTag
    ]: c.Tree =
      materializeOp1Gen[Op1Macro[_, _], N, S1].usingFuncName
  }
}

/*******************************************************************************************************/
/********************************************************************************************************
  * Two arguments type function macro
  *******************************************************************************************************/
trait Op2Macro[N <: String with Singleton, S1, S2] extends SingletonTypeExpr

@bundle
object Op2Macro {
  implicit def call[N <: String with Singleton, S1, S2]: Op2Macro[N, S1, S2] =
    macro Macro.impl[N, S1, S2]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        N <: String with Singleton: c.WeakTypeTag,
        S1: c.WeakTypeTag,
        S2: c.WeakTypeTag
    ]: c.Tree =
      materializeOp2Gen[Op2Macro[_, _, _], N, S1, S2].usingFuncName
  }
}

/*******************************************************************************************************/
trait ToNatMacro[S1] extends Nat

@bundle
object ToNatMacro {
  implicit def call[S1]: ToNatMacro[S1] =
    macro Macro.impl[S1]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        S1: c.WeakTypeTag
    ]: c.Tree =
      materializeToNat[ToNatMacro[_], S1].usingFuncName
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
