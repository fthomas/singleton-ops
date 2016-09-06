package singleton.ops.impl

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait SingletonTypeExpr extends Serializable {
  type BaseType
  type Out <: BaseType with Singleton
  val value : Out {}
  //For debugging only
  def outTypeName : String = {
    if (value.isInstanceOf[Int]) "Int"
    else "Unknown"
  }
//  match {
//    case (p.isInstanceOf[Int]) => "Int"
//    case (_ : Long with Singleton) => "Long"
//    case (_ : Double with Singleton) => "Double"
//    case (_ : String with Singleton) => "String"
//    case _ => "Unknown"
//  }
}


trait SingletonTypeExprBase[B] extends SingletonTypeExpr {type BaseType = B}

trait SingletonTypeExprInt extends SingletonTypeExprBase[Int]
trait SingletonTypeExprLong extends SingletonTypeExprBase[Long]
trait SingletonTypeExprDouble extends SingletonTypeExprBase[Double]
trait SingletonTypeExprString extends SingletonTypeExprBase[String]


trait SingletonTypeValue[S] extends SingletonTypeExpr

sealed trait SingletonTypeValueInt[S <: Int with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprInt {type Out = S}
sealed trait SingletonTypeValueLong[S <: Long with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprLong {type Out = S}
sealed trait SingletonTypeValueDouble[S <: Double with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprDouble {type Out = S}
sealed trait SingletonTypeValueString[S <: String with Singleton] extends SingletonTypeValue[S] with SingletonTypeExprString {type Out = S}


//trait Splitter[S] {
//  type BaseType
//  type Out <: BaseType with Singleton
//  val value : Out {}
//}

//object Splitter {
//  def apply[S](implicit ret : Splitter[S]) : Aux[S, ret.BaseType, ret.Out] = ret
//  type Aux[S, Ret_BaseType, Ret_Out <: Ret_BaseType with Singleton] = Splitter[S] {type BaseType = Ret_BaseType; type Out = Ret_Out}
//  implicit def impl[S](implicit m : SplitterMacro[S]) : Aux[S, m.BaseType, m.Out] = m
//}

@bundle
object SingletonTypeValueMacro {
  implicit def call[S]: SingletonTypeValue[S] {type BaseType = Int} =
  macro Macro.impl[S]

  final class Macro(val c: whitebox.Context) extends Macros {
    def impl[S: c.WeakTypeTag]: c.Tree = materializeSplitter[SingletonTypeValue[_], S].usingFunction
  }
}


//
//object SingletonTypeValue {
//  def apply[S](implicit ret : SingletonTypeValue[S]) : Aux[S, ret.BaseType, ret.Out] = ret
//  type Aux[S, Ret_BaseType, Ret_Out <: Ret_BaseType with Singleton] = SingletonTypeValue[S] {type BaseType = Ret_BaseType; type Out = Ret_Out}
//  implicit def implInt[S <: Int with Singleton]
//  (implicit v: ValueOf[S]) : SingletonTypeValue[S] =
//    new SingletonTypeValue[S] {type BaseType = Int; type Out = S; val value : Out = valueOf[S]}
//
//    implicit def implInt2[S <: Int with Singleton]
//    (implicit v: ValueOf[S], di : DummyImplicit) : SingletonTypeValueInt[S] =
//      new SingletonTypeValueInt[S] {val value : Out = valueOf[S]}
////  implicit def implLong[S <: Long with Singleton]
////  (implicit v: ValueOf[S], di : DummyImplicit) : SingletonTypeValueLong[S] =
////    new SingletonTypeValueLong[S] {val value : Out = valueOf[S]}
////
////  implicit def implDouble[S <: Double with Singleton]
////  (implicit v: ValueOf[S], di : DummyImplicit, di2 : DummyImplicit) : SingletonTypeValueDouble[S] =
////    new SingletonTypeValueDouble[S] {val value : Out = valueOf[S]}
////
////  implicit def implString[S <: String with Singleton]
////  (implicit v: ValueOf[S], di : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit) : SingletonTypeValueString[S] =
////    new SingletonTypeValueString[S] {val value : Out = valueOf[S]}
//}



trait Op {
  type Out
  val value: Out {}
}

trait Op2[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] extends SingletonTypeExpr
trait Op2Int[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] extends Op2[T1,S1,T2,S2] with SingletonTypeExprInt
trait Op2Long[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] extends Op2[T1,S1,T2,S2] with SingletonTypeExprLong
trait Op2Double[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] extends Op2[T1,S1,T2,S2] with SingletonTypeExprDouble
trait Op2String[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] extends Op2[T1,S1,T2,S2] with SingletonTypeExprString

trait SumMacroInt[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton] extends Op2Int[T1,S1,T2,S2]

@bundle
object SumMacroInt {
  implicit def call[T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton]
  (implicit nt1: Numeric[T1], nt2: Numeric[T2]): SumMacroInt[T1,S1,T2,S2] =
    macro Macro.impl[T1,S1,T2,S2]

  final class Macro(val c: whitebox.Context) extends Macros {
    def impl[
      T1: c.WeakTypeTag,
      S1 <: T1 with Singleton: c.WeakTypeTag,
      T2: c.WeakTypeTag,
      S2 <: T2 with Singleton: c.WeakTypeTag
    ](nt1: c.Expr[Numeric[T1]], nt2: c.Expr[Numeric[T2]]): c.Tree =
      materializeOp3[SumMacroInt[_, _, _, _], T1,S1,T2,S2]
        .usingFunction(evalTyped(nt1).plus)
  }
}
