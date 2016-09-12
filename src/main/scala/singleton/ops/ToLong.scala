package singleton.ops

import singleton.ops.impl._

sealed trait ToLong[P1 <: SingletonTypeExpr] extends SingletonTypeFunc1[P1]

object ToLong {
  def apply[P1 <: SingletonTypeExpr](p1: P1, op: SingletonTypeExprBase[Long]) =
    new ToLong[P1] with SingletonTypeExprBase[Long] {
      type Out = op.Out
      val value: Out {} = op.value
    }

  type Aux[
      P1 <: SingletonTypeExpr,
      P1_BaseType,
      P1_Out <: P1_BaseType with Singleton,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ] = ToLong[P1] {
    type BaseType = Ret_BaseType
    type Out = Ret_Out
  }

  implicit def implInt[
      P1 <: SingletonTypeExpr,
      P1_BaseType <: Int,
      P1_Out <: P1_BaseType with Singleton,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1: P1,
    p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    op: ToLongMacro[Long, P1_BaseType, P1_Out]): Aux[
    P1,
    P1_BaseType,
    P1_Out,
    op.BaseType,
    op.Out] with SingletonTypeExprBase[Long] = ToLong[P1](p1, op)

  implicit def implLong[
      P1 <: SingletonTypeExpr,
      P1_BaseType <: Long,
      P1_Out <: P1_BaseType with Singleton,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1: P1,
    p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    op: ToLongMacro[Long, P1_BaseType, P1_Out]): Aux[
    P1,
    P1_BaseType,
    P1_Out,
    op.BaseType,
    op.Out] with SingletonTypeExprBase[Long] = ToLong[P1](p1, op)
}
