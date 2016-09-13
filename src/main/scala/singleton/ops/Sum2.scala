package singleton.ops

import singleton.ops.impl._

sealed trait Sum2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
    extends SingletonTypeFunc2[P1, P2]

trait Func2StaticAux[
    F[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] <: SingletonTypeFunc2[
      P1,
      P2]
] {
  type Aux[
      P1 <: SingletonTypeExpr,
      P2 <: SingletonTypeExpr,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ] = F[P1, P2] {
    type BaseType = Ret_BaseType
    type Out = Ret_Out
  }
}


object Sum2 extends Func2StaticAux[Sum2] { //extends SingletonTypeFunc2Static("+") {

  implicit def impl[
      P1 <: SingletonTypeExpr,
      P2 <: SingletonTypeExpr,
      P1_BaseType,
      P1_Out <: P1_BaseType with Singleton,
      P2_BaseType,
      P2_Out <: P2_BaseType with Singleton,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ](implicit
    p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    p2_ret: Repeater.Aux[P2, P2_BaseType, P2_Out],
    op: Op2Macro["Plus",P1_BaseType, P1_Out, P2_BaseType, P2_Out])
    : Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExpr =
    new Sum2[P1, P2] {
      type BaseType = op.BaseType
      type Out = op.Out
      val value: Out {} = op.value
    }
}
