package singleton.ops.impl

import macrocompat.bundle
import singleton.ops.ToLong
import singleton.ops.ToInt

import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait SingletonTypeExpr extends Serializable {
  type BaseType
  type Out <: BaseType with Singleton
  type OutInt <: Int with Singleton
  type OutLong <: Long with Singleton
  type OutDouble <: Double with Singleton
  type OutString <: String with Singleton
  val value: Out {}
  //For debugging only
  def outTypeName: String = {
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

trait SingletonTypeExprBase[B] extends SingletonTypeExpr { type BaseType = B }

trait SingletonTypeExprInt extends SingletonTypeExprBase[Int] {
  type OutInt = Out
}
trait SingletonTypeExprLong extends SingletonTypeExprBase[Long] {
  type OutLong = Out
}
trait SingletonTypeExprDouble extends SingletonTypeExprBase[Double] {
  type OutDouble = Out
}
trait SingletonTypeExprString extends SingletonTypeExprBase[String] {
  type OutString = Out
}

trait Extractor[P <: SingletonTypeExpr] extends SingletonTypeExpr

object Extractor {
  type Aux[P <: SingletonTypeExpr,
           Ret_BaseType,
           Ret_Out <: Ret_BaseType with Singleton] =
    Extractor[P] { type BaseType = Ret_BaseType; type Out = Ret_Out }
  implicit def impl[P <: SingletonTypeExpr, Ret_BaseType, Ret_Out](
      implicit p: P): Aux[P, p.BaseType, p.Out] =
    new Extractor[P] {
      type BaseType = p.BaseType; type Out = p.Out; val value: Out = p.value
    }
}

trait SingletonTypeValue[S] extends SingletonTypeExpr

sealed trait SingletonTypeValueInt[S <: Int with Singleton]
    extends SingletonTypeValue[S]
    with SingletonTypeExprInt {
  type Out = S
}
sealed trait SingletonTypeValueLong[S <: Long with Singleton]
    extends SingletonTypeValue[S]
    with SingletonTypeExprLong {
  type Out = S
}
sealed trait SingletonTypeValueDouble[S <: Double with Singleton]
    extends SingletonTypeValue[S]
    with SingletonTypeExprDouble {
  type Out = S
}
sealed trait SingletonTypeValueString[S <: String with Singleton]
    extends SingletonTypeValue[S]
    with SingletonTypeExprString {
  type Out = S
}

object SingletonTypeValue {

  implicit def implExpr[P1 <: SingletonTypeExpr](
      implicit op: Extractor[P1]): SingletonTypeValue[P1] {
    type BaseType = op.BaseType; type Out = op.Out
  } =
    new SingletonTypeValue[P1] {
      type BaseType = op.BaseType; type Out = op.Out; val value: Out = op.value
    }

  implicit def implInt[S <: Int with Singleton](
      implicit v: ValueOf[S]): SingletonTypeValueInt[S] =
    new SingletonTypeValueInt[S] { val value: Out = valueOf[S] }

  implicit def implLong[S <: Long with Singleton](
      implicit v: ValueOf[S],
      di: DummyImplicit): SingletonTypeValueLong[S] =
    new SingletonTypeValueLong[S] { val value: Out = valueOf[S] }

  implicit def implDouble[S <: Double with Singleton](
      implicit v: ValueOf[S],
      di: DummyImplicit,
      di2: DummyImplicit): SingletonTypeValueDouble[S] =
    new SingletonTypeValueDouble[S] { val value: Out = valueOf[S] }

  implicit def implString[S <: String with Singleton](
      implicit v: ValueOf[S],
      di: DummyImplicit,
      di2: DummyImplicit,
      di3: DummyImplicit): SingletonTypeValueString[S] =
    new SingletonTypeValueString[S] { val value: Out = valueOf[S] }
}

trait SingletonTypeFunc1[P1 <: SingletonTypeExpr] extends SingletonTypeExpr
trait SingletonTypeFunc2[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr]
    extends SingletonTypeExpr

trait Op {
  type Out
  val value: Out {}
}

trait Op1[B, T1, S1 <: T1 with Singleton] extends SingletonTypeExprBase[B]
trait Op2[B, T1, S1 <: T1 with Singleton, T2, S2 <: T2 with Singleton]
    extends SingletonTypeExprBase[B]
