package singleton.ops

import singleton.ops.impl._

sealed trait ToInt[P1 <: SingletonTypeExpr] extends SingletonTypeFunc1[P1]

object ToInt {
  def apply[P1 <: SingletonTypeExpr](p1: P1, op: SingletonTypeExprBase[Int]) =
    new ToInt[P1] with SingletonTypeExprBase[Int] {
      type Out = op.Out
      val value: Out {} = op.value
    }

  type Aux[
      P1 <: SingletonTypeExpr,
      P1_BaseType,
      P1_Out <: P1_BaseType with Singleton,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ] = ToInt[P1] {
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
    op: ToIntMacro[Int, P1_BaseType, P1_Out]): Aux[
    P1,
    P1_BaseType,
    P1_Out,
    op.BaseType,
    op.Out] with SingletonTypeExprBase[Int] = ToInt[P1](p1, op)

  implicit def implLong[
      P1 <: SingletonTypeExpr,
      P1_BaseType <: Long,
      P1_Out <: P1_BaseType with Singleton,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1: P1,
    p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    op: ToIntMacro[Int, P1_BaseType, P1_Out]): Aux[
    P1,
    P1_BaseType,
    P1_Out,
    op.BaseType,
    op.Out] with SingletonTypeExprBase[Int] = ToInt[P1](p1, op)
}
