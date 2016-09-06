package singleton.ops

import macrocompat.bundle
import shapeless.TestMacros

import scala.reflect.macros.whitebox
import singleton.ops.impl._


abstract class SingletonTypeFunc2Static(funcName : String) {
  final def paramMismatchException(p1 : SingletonTypeExpr, p2 : SingletonTypeExpr) =
    new RuntimeException("Unsupported <"+ p1.outTypeName + "> " + funcName + " <" + p2.outTypeName + ">")
}


sealed trait SingletonTypeFunc2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends SingletonTypeExpr

sealed trait Sum2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends SingletonTypeFunc2[P1, P2]
sealed trait Sum2Int[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends Sum2[P1, P2] with SingletonTypeExprInt
sealed trait Sum2Long[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends Sum2[P1, P2] with SingletonTypeExprLong
sealed trait Sum2Double[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends Sum2[P1, P2] with SingletonTypeExprDouble

trait Extractor[P <: SingletonTypeExpr] {
  type BaseType
  type Out <: BaseType with Singleton
}

object Extractor {
  type Aux[P <: SingletonTypeExpr, Ret_BaseType, Ret_Out <: Ret_BaseType with Singleton] =
    Extractor[P] {type BaseType = Ret_BaseType; type Out = Ret_Out}
  implicit def impl[P <: SingletonTypeExpr, Ret_BaseType, Ret_Out](implicit p : P) : Aux[P, p.BaseType, p.Out] =
    new Extractor[P] {type BaseType = p.BaseType; type Out = p.Out}
}


object Sum2Int {//extends SingletonTypeFunc2Static("+") {
//  def apply[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
//  (implicit ret: Sum2[P1, P2]): Aux[P1, P2, ret.Out] = ret
//
//  private def implInt[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr](p1: P1, p2: P2) : Sum2Int[P1, P2] = new Sum2Int[P1, P2] {
//    type Out = 1
//    val value : Out {} = 1
//  }
//
//  private def implLong[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr](p1: P1, p2: P2) : Sum2Long[P1, P2] = new Sum2Long[P1, P2] {
//    type Out = 2L
//    val value : Out {} = 2L
//  }
//
//  private def implDouble[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr](p1: P1, p2: P2) : Sum2Double[P1, P2] = new Sum2Double[P1, P2] {
//    type Out = 3.0
//    val value : Out {} = 3.0
//  }
//

  type Aux[
    P1 <: SingletonTypeExpr,
    P2 <: SingletonTypeExpr,
    P1_BaseType,
    P1_Out <: P1_BaseType with Singleton,
    P2_BaseType,
    P2_Out <: P2_BaseType with Singleton,
    Ret_BaseType,
    Ret_Out <: Ret_BaseType with Singleton
  ] = Sum2Int[P1, P2] {
    type BaseType = Ret_BaseType
    type Out = Ret_Out
  }

  implicit def impl[
    P1 <: SingletonTypeExpr,
    P2 <: SingletonTypeExpr,
    P1_BaseType,
    P1_Out <: P1_BaseType with Singleton,
    P2_BaseType,
    P2_Out <: P2_BaseType with Singleton,
    Ret_BaseType,
    Ret_Out <: Ret_BaseType with Singleton
  ](implicit p1 : P1, p2: P2, p1_ret : Extractor.Aux[P1,P1_BaseType,P1_Out], p2_ret : Extractor.Aux[P2,P2_BaseType,P2_Out], op : SumMacroInt[P1_BaseType, P1_Out, P2_BaseType, P2_Out]) :
    Aux[P1, P2, P1_BaseType, P1_Out, P2_BaseType, P2_Out, op.BaseType, op.Out] = {
    new Sum2Int[P1, P2] {
      type Out = op.Out
      val value : Out {} = op.value
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
  type +[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] = Sum2Int[P1, P2]
  type @@[S] = SingletonTypeValue[S]
}

import infixops._







//trait Foo {
//  type Out
//}
//
//trait ExtendedFoo extends Foo
//
//object ExtendedFoo {
//  def implFoo() : ExtendedFoo = new ExtendedFoo {
//    type Out = 1
//  }
//}
//
//object TestFoo {
//  val foo = ExtendedFoo.implFoo()
//  val not_working : foo.Out = 1
//}







trait Opy[Upper] extends Serializable {
  type Out <: Upper with Singleton
  val value: Out {}
}

trait Sum[T, A <: T with Singleton, B <: T with Singleton] extends Opy[T]

//object Sum {
//  def apply[T, A <: T with Singleton, B <: T with Singleton](
//      implicit sum: Sum[T, A, B]): Aux[T, A, B, sum.Out] = sum
//
//  def apply[A <: Int with Singleton, B <: Int with Singleton](
//      implicit sum: Sum[Int, A, B],
//      di1: DummyImplicit): Aux[Int, A, B, sum.Out] = sum
//
//  def apply[A <: Double with Singleton, B <: Double with Singleton](
//      implicit sum: Sum[Double, A, B],
//      di1: DummyImplicit,
//      di2: DummyImplicit): Aux[Double, A, B, sum.Out] = sum
//
//  type Aux[T,
//           A <: T with Singleton,
//           B <: T with Singleton,
//           C <: T with Singleton] = Sum[T, A, B] { type Out = C }
//
//  implicit def macroCall[T, A <: T with Singleton, B <: T with Singleton](
//      implicit nt: Numeric[T]): Sum[T, A, B] =
//    macro Macro.impl[Sum[_, _, _], T, A, B]
//
//  @bundle
//  final class Macro(val c: whitebox.Context) extends Macros {
//    def impl[F <: Serializable: c.WeakTypeTag,
//             T: c.WeakTypeTag,
//             A <: T with Singleton: c.WeakTypeTag,
//             B <: T with Singleton: c.WeakTypeTag](
//        nt: c.Expr[Numeric[T]]): c.Tree =
//      materializeOp3[F, T1, S1, B].usingFunction(evalTyped(nt).plus)
//  }
//}
//abstract class Plus[A <: Op, B <: Op](val a: A, b: B) extends Op {
//  type Upper = a.Upper
//}
//
//object Plus extends Op2CompanionPlus[Plus] {
//  implicit def materialize[A <: Op, B <: Op](a: A, b: B) : Plus[A, B] = {
//
//    new Plus[A,B](a, b) {
//      type Out = a.Out
//      val value: Out = a.value
//    }
//
//  }
//  //    //val materializePlus[a.Upper, a.Out, b.Out]
//  //
//  //  def materializePlus[T, A <: T, B <: T](
//  //      implicit nt: Numeric[T]
//  //  ): Plus[A, B] = macro PlusMacro.materialize[T, A, B]
//  //
//  //  @bundle
//  //  final class PlusMacro(val c: whitebox.Context) extends Macros {
//  //    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
//  //        nt: c.Expr[Numeric[T]]
//  //    ): c.Tree =
//  //      materializeOp2Plus[Plus, A, B].usingFunction(evalTyped(nt).plus)
//  //  }
//}
//
