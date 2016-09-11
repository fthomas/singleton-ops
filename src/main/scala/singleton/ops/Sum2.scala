package singleton.ops


import singleton.ops.impl._

sealed trait Sum2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends SingletonTypeFunc2[P1, P2]


object Sum2 {//extends SingletonTypeFunc2Static("+") {
def apply[P1 <: SingletonTypeExpr,P2 <: SingletonTypeExpr](p1: P1, p2: P2, op : SingletonTypeExprBase[Int]) =
new Sum2[P1, P2] with SingletonTypeExprInt {
  type Out = op.Out
  val value : Out {} = op.value
}
  def apply[P1 <: SingletonTypeExpr,P2 <: SingletonTypeExpr](p1: P1, p2: P2, op : SingletonTypeExprBase[Long])(implicit di1: DummyImplicit) =
    new Sum2[P1, P2] with SingletonTypeExprLong {
      type Out = op.Out
      val value : Out {} = op.value
    }

  type Aux[
  P1 <: SingletonTypeExpr,
  P2 <: SingletonTypeExpr,
  Ret_BaseType,
  Ret_Out <: Ret_BaseType with Singleton
  ] = Sum2[P1, P2] {
    type BaseType = Ret_BaseType
    type Out = Ret_Out
  }

  implicit def implIntInt[
  P1 <: SingletonTypeExpr,
  P2 <: SingletonTypeExpr,
  P1_BaseType <: Int,
  P1_Out <: P1_BaseType with Singleton,
  P2_BaseType <: Int,
  P2_Out <: P2_BaseType with Singleton,
  Ret_BaseType,
  Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1 : P1, p2: P2, p1_ret : Extractor.Aux[P1,P1_BaseType,P1_Out], p2_ret : Extractor.Aux[P2,P2_BaseType,P2_Out], op : SumMacro[Int, P1_BaseType, P1_Out, P2_BaseType, P2_Out]) :
  Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprInt = Sum2[P1,P2](p1,p2,op)

  implicit def implLongLong[
  P1 <: SingletonTypeExpr,
  P2 <: SingletonTypeExpr,
  P1_BaseType <: Long,
  P1_Out <: P1_BaseType with Singleton,
  P2_BaseType <: Long,
  P2_Out <: P2_BaseType with Singleton,
  Ret_BaseType,
  Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1 : P1, p2: P2, p1_ret : Extractor.Aux[P1,P1_BaseType,P1_Out], p2_ret : Extractor.Aux[P2,P2_BaseType,P2_Out], op : SumMacro[Long, P1_BaseType, P1_Out, P2_BaseType, P2_Out]) :
  Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprLong = Sum2[P1,P2](p1,p2,op)

  implicit def implLongInt[
  P1 <: SingletonTypeExpr,
  P2 <: SingletonTypeExpr,
  P1_BaseType <: Long,
  P1_Out <: P1_BaseType with Singleton,
  P2_BaseType <: Int,
  P2_Out <: P2_BaseType with Singleton,
  PM_BaseType,
  PM_Out <: PM_BaseType with Singleton,
  Ret_BaseType,
  Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1 : P1, p2: P2, p1_ret : Extractor.Aux[P1,P1_BaseType,P1_Out], p2_ret : ToLong.Aux[P2,P2_BaseType,P2_Out,PM_BaseType,PM_Out], op : SumMacro[Long, P1_BaseType, P1_Out, PM_BaseType, PM_Out]) :
  Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprLong = Sum2[P1,P2](p1,p2,op)

  implicit def implIntLong[
  P1 <: SingletonTypeExpr,
  P2 <: SingletonTypeExpr,
  P1_BaseType <: Int,
  P1_Out <: P1_BaseType with Singleton,
  P2_BaseType <: Long,
  P2_Out <: P2_BaseType with Singleton,
  PM_BaseType,
  PM_Out <: PM_BaseType with Singleton,
  Ret_BaseType,
  Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1 : P1, p2: P2, p1_ret : ToLong.Aux[P1,P1_BaseType,P1_Out,PM_BaseType,PM_Out], p2_ret : Extractor.Aux[P2,P2_BaseType,P2_Out], op : SumMacro[Long, PM_BaseType, PM_Out, P2_BaseType, P2_Out]) :
  Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprLong = Sum2[P1,P2](p1,p2,op)
}
