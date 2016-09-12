package singleton.ops

import singleton.ops.impl._

sealed trait Sum2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
    extends SingletonTypeFunc2[P1, P2]

trait Func2StaticAux[
    F[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] <: SingletonTypeFunc2[
      P1,
      P2],
    OP[B, T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] <: Op2[B,
                                                                           T1,
                                                                           S1,
                                                                           T2,
                                                                           S2]
] {
  type OpMacro[B, T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] =
    OP[B, T1, S1, T2, S2]
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

//trait Func2StaticCreateInt[F[P1 <: SingletonTypeExpr,P2 <: SingletonTypeExpr] <: SingletonTypeFunc2[P1,P2]] {
//  def createAsInt[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr](p1: P1, p2: P2, op : SingletonTypeExprBase[Int]) :
//  F[P1, P2] with SingletonTypeExprInt
//}
//
//trait Func2StaticSupportIntInt[
//F[P1 <: SingletonTypeExpr,P2 <: SingletonTypeExpr] <: SingletonTypeFunc2[P1,P2],
//OP[B, T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] <: Op2[B,T1,S1,T2,S2]
//] extends Func2StaticAux[F] with Func2StaticCreateInt[F]{
//  implicit def implIntInt[
//  P1 <: SingletonTypeExpr,
//  P2 <: SingletonTypeExpr,
//  P1_BaseType <: Int,
//  P1_Out <: P1_BaseType with Singleton,
//  P2_BaseType <: Int,
//  P2_Out <: P2_BaseType with Singleton,
//  Ret_BaseType,
//  Ret_Out <: Ret_BaseType with Singleton
//  ](implicit p1 : P1, p2: P2, p1_ret : Extractor.Aux[P1,P1_BaseType,P1_Out], p2_ret : Extractor.Aux[P2,P2_BaseType,P2_Out], op : OP[Int, P1_BaseType, P1_Out, P2_BaseType, P2_Out]) :
//  Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprInt = createAsInt[P1,P2](p1,p2,op)
//}

object Sum2 extends Func2StaticAux[Sum2, SumMacro] { //extends SingletonTypeFunc2Static("+") {
  def apply[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr](
      p1: P1,
      p2: P2,
      op: SingletonTypeExprBase[Int]) =
    new Sum2[P1, P2] with SingletonTypeExprBase[Int] {
      type Out = op.Out
      val value: Out {} = op.value
    }
  def apply[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr](
      p1: P1,
      p2: P2,
      op: SingletonTypeExprBase[Long])(implicit di1: DummyImplicit) =
    new Sum2[P1, P2] with SingletonTypeExprBase[Long] {
      type Out = op.Out
      val value: Out {} = op.value
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
  ](implicit p1: P1,
    p2: P2,
    p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    p2_ret: Repeater.Aux[P2, P2_BaseType, P2_Out],
    op: OpMacro[Int, P1_BaseType, P1_Out, P2_BaseType, P2_Out])
    : Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprBase[Int] =
    Sum2[P1, P2](p1, p2, op)

  implicit def implLongLong[
      P1 <: SingletonTypeExpr,
      P2 <: SingletonTypeExpr,
      P1_BaseType <: Long,
      P1_Out <: P1_BaseType with Singleton,
      P2_BaseType <: Long,
      P2_Out <: P2_BaseType with Singleton,
      Ret_BaseType,
      Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1: P1,
    p2: P2,
    p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    p2_ret: Repeater.Aux[P2, P2_BaseType, P2_Out],
    op: OpMacro[Long, P1_BaseType, P1_Out, P2_BaseType, P2_Out])
    : Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprBase[Long] =
    Sum2[P1, P2](p1, p2, op)

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
  ](implicit p1: P1,
    p2: P2,
    p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    p2_ret: ToLong.Aux[P2, P2_BaseType, P2_Out, PM_BaseType, PM_Out],
    op: OpMacro[Long, P1_BaseType, P1_Out, PM_BaseType, PM_Out])
    : Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprBase[Long] =
    Sum2[P1, P2](p1, p2, op)

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
  ](implicit p1: P1,
    p2: P2,
    p1_ret: ToLong.Aux[P1, P1_BaseType, P1_Out, PM_BaseType, PM_Out],
    p2_ret: Repeater.Aux[P2, P2_BaseType, P2_Out],
    op: OpMacro[Long, PM_BaseType, PM_Out, P2_BaseType, P2_Out])
    : Aux[P1, P2, op.BaseType, op.Out] with SingletonTypeExprBase[Long] =
    Sum2[P1, P2](p1, p2, op)
}
