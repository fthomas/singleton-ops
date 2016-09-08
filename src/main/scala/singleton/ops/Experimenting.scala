package singleton.ops

import macrocompat.bundle
import shapeless.TestMacros

import scala.reflect.macros.whitebox
import singleton.ops.impl._

abstract class SingletonTypeFunc2Static(funcName: String) {
  final def paramMismatchException(p1: SingletonTypeExpr,
                                   p2: SingletonTypeExpr) =
    new RuntimeException(
      "Unsupported <" + p1.outTypeName + "> " + funcName + " <" + p2.outTypeName + ">")
}

sealed trait SingletonTypeFunc2[
    P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
    extends SingletonTypeExpr

sealed trait Sum2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
    extends SingletonTypeFunc2[P1, P2] //{type Out <: Int with Singleton}
sealed trait Sum2Int[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
    extends Sum2[P1, P2]
    with SingletonTypeExprInt {}
//sealed trait Sum2Long[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
//    extends Sum2[P1, P2]
//    with SingletonTypeExprLong {}
//sealed trait Sum2Double[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
//    extends Sum2[P1, P2]
//    with SingletonTypeExprDouble

object Sum2 { //extends SingletonTypeFunc2Static("+") {
  type AuxIntInt[
      P1 <: SingletonTypeExpr,
      P2 <: SingletonTypeExpr,
      P1_BaseType <: Int,
      P1_Out,
      P2_BaseType <: Int,
      P2_Out,
//      Ret_BaseType,
      Ret_Out
  ] = Sum2Int[P1, P2] {
    type BaseType = Int
    type Out = Ret_Out
  }

  implicit def implIntInt[
      P1 <: SingletonTypeExpr,
      P2 <: SingletonTypeExpr,
      P1_BaseType <: Int,
      P1_Out,
      P2_BaseType <: Int,
      P2_Out,
//      Ret_BaseType,
      Ret_Out
  ](implicit p1: P1,
    p2: P2,
    p1_ret: Extractor.Aux[P1, P1_BaseType, P1_Out],
    p2_ret: Extractor.Aux[P2, P2_BaseType, P2_Out],
    op: SumMacro[Int, P1_BaseType, P1_Out, P2_BaseType, P2_Out]): AuxIntInt[
    P1,
    P2,
    P1_BaseType,
    P1_Out,
    P2_BaseType,
    P2_Out,
//      op.BaseType,
    op.Out
  ] = {
    new Sum2Int[P1, P2] {
//      type BaseType = Int
      type Out = op.Out
      val value: Out {} = op.value
    }
//    (p1, p2) match {
//      case (_ : SingletonTypeExprInt, _ : SingletonTypeExprInt) => implInt[P1, P2](p1, p2)
//      case (_ : SingletonTypeExprLong, _ : SingletonTypeExprLong) => implLong[P1, P2](p1, p2)
//      case (_ : SingletonTypeExprInt, _ : SingletonTypeExprLong) => implLong[P1, P2](p1, p2)
//      case (_ : SingletonTypeExprLong, _ : SingletonTypeExprInt) => implLong[P1, P2](p1, p2)
//      case (_ : SingletonTypeExprDouble, _ : SingletonTypeExprDouble) => implDouble[P1, P2](p1, p2)
//      case _ =>
//        throw paramMismatchException(p1,p2)
//    }
  }
}


object infixops {
  type +[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] = Sum2[P1, P2]
  type @@[S] = SingletonTypeValue[S]
}

import infixops._

trait Opy[Upper] extends Serializable {
  type Out <: Upper with Singleton
  val value: Out {}
}

trait Sum[T, A <: T with Singleton, B <: T with Singleton] extends Opy[T]
