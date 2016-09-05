package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._



sealed trait SingletonTypeExpr extends Serializable {
  type BaseType
  type Out <: BaseType with Singleton
  val value : Out {}
  val outTypeName : String
}


trait SingletonTypeExprBase[B] extends SingletonTypeExpr {type BaseType = B}

trait SingletonTypeExprInt extends SingletonTypeExprBase[Int] {val outTypeName : String = "Int"}

object SingletonTypeExprInt {
  def apply(implicit ret: SingletonTypeExprInt): Aux[ret.Out] = ret
  type Aux[Ret_Out <: Int with Singleton] = SingletonTypeExprInt {type Out = Ret_Out}
  implicit def impl[Ret_Out <: Int with Singleton](implicit r : ValueOf[Ret_Out]) :
  SingletonTypeExprInt {type Out = Ret_Out} = {
    new SingletonTypeExprInt {
      type Out = Ret_Out
      val value : Out = valueOf[Ret_Out]
    }
  }
}

trait SingletonTypeExprLong extends SingletonTypeExprBase[Long] {val outTypeName : String = "Long"}
trait SingletonTypeExprDouble extends SingletonTypeExprBase[Double] {val outTypeName : String = "Double"}
trait SingletonTypeExprString extends SingletonTypeExprBase[String] {val outTypeName : String = "String"}


trait SingletonTypeValue[S <: Singleton] extends SingletonTypeExpr

sealed trait SingletonTypeValueInt[S <: Int with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprInt {type Out = S}
sealed trait SingletonTypeValueLong[S <: Long with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprLong {type Out = S}
sealed trait SingletonTypeValueDouble[S <: Double with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprDouble {type Out = S}
sealed trait SingletonTypeValueString[S <: String with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprString {type Out = S}


object SingletonTypeValue {
  implicit def implInt[S <: Int with Singleton]
  (implicit v: ValueOf[S]) : SingletonTypeValueInt[S] =
    new SingletonTypeValueInt[S] {val value : Out = valueOf[S]}

  implicit def implLong[S <: Long with Singleton]
  (implicit v: ValueOf[S], di : DummyImplicit) : SingletonTypeValueLong[S] =
    new SingletonTypeValueLong[S] {val value : Out = valueOf[S]}

  implicit def implDouble[S <: Double with Singleton]
  (implicit v: ValueOf[S], di : DummyImplicit, di2 : DummyImplicit) : SingletonTypeValueDouble[S] =
    new SingletonTypeValueDouble[S] {val value : Out = valueOf[S]}

  implicit def implString[S <: String with Singleton]
  (implicit v: ValueOf[S], di : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit) : SingletonTypeValueString[S] =
    new SingletonTypeValueString[S] {val value : Out = valueOf[S]}
}



abstract class SingletonTypeFunc2Static(funcName : String) {
  final def paramMismatchException(p1 : SingletonTypeExpr, p2 : SingletonTypeExpr) =
    new RuntimeException("Unsupported <"+ p1.outTypeName + "> " + funcName + " <" + p2.outTypeName + ">")
}


sealed trait SingletonTypeFunc2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends SingletonTypeExpr

sealed trait Sum2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends SingletonTypeFunc2[P1, P2]
sealed trait Sum2Int[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends Sum2[P1, P2] with SingletonTypeExprInt
sealed trait Sum2Long[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends Sum2[P1, P2] with SingletonTypeExprLong
sealed trait Sum2Double[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] extends Sum2[P1, P2] with SingletonTypeExprDouble


object Sum2 {//extends SingletonTypeFunc2Static("+") {
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
//  trait Extractor[P <: SingletonTypeExpr] {
//    type Out
//  }
//
//  object Extractor {
//    def apply[P <: SingletonTypeExpr](implicit ret: Extractor[P]): Aux[P, ret.Out] = ret
//    type Aux[P <: SingletonTypeExpr, Ret_Out] = Extractor[P] {type Out = Ret_Out}
//    implicit def impl[P <: SingletonTypeExpr, Ret_Out](implicit r : Aux[P, Ret_Out]) : Extractor[P] {type Out = r.Out} =
//      new Extractor[P] {type Out = r.Out}
//  }

  type Aux[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr, P1_Out, P2_Out, Ret_Out] = Sum2[P1, P2]

  implicit def impl[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr, P1_Out, P2_Out, Ret_Out](implicit p1 : P1) : Sum2[P1, P2] = { //implicit r1 : Aux1[P1_Out], r2 : Aux2[P2_Out], op : SumMacro[Int, P1_Out, P2_Out]
    new Sum2Int[P1, P2] {
      type Out = 1
      val value : Out {} = 1
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
  type @@[S <: Singleton] = SingletonTypeValue[S]
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

object Sum {
  def apply[T, A <: T with Singleton, B <: T with Singleton](
      implicit sum: Sum[T, A, B]): Aux[T, A, B, sum.Out] = sum

  def apply[A <: Int with Singleton, B <: Int with Singleton](
      implicit sum: Sum[Int, A, B],
      di1: DummyImplicit): Aux[Int, A, B, sum.Out] = sum

  def apply[A <: Double with Singleton, B <: Double with Singleton](
      implicit sum: Sum[Double, A, B],
      di1: DummyImplicit,
      di2: DummyImplicit): Aux[Double, A, B, sum.Out] = sum

  type Aux[T,
           A <: T with Singleton,
           B <: T with Singleton,
           C <: T with Singleton] = Sum[T, A, B] { type Out = C }

  implicit def macroCall[T, A <: T with Singleton, B <: T with Singleton](
      implicit nt: Numeric[T]): Sum[T, A, B] =
    macro Macro.impl[Sum[_, _, _], T, A, B]

  @bundle
  final class Macro(val c: whitebox.Context) extends Macros {
    def impl[F <: Serializable: c.WeakTypeTag,
             T: c.WeakTypeTag,
             A <: T with Singleton: c.WeakTypeTag,
             B <: T with Singleton: c.WeakTypeTag](
        nt: c.Expr[Numeric[T]]): c.Tree =
      materializeOp3[F, T, A, B].usingFunction(evalTyped(nt).plus)
  }
}
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
