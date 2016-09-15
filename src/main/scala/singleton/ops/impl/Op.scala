package singleton.ops.impl

import macrocompat.bundle
import shapeless.Nat

import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait SingletonTypeExpr extends Serializable {
  type Out
  type OutWide
  type OutNat <: Nat
  type OutInt <: Int with Singleton
  type OutLong <: Long with Singleton
  type OutDouble <: Double with Singleton
  type OutString <: String with Singleton
  type OutBoolean <: Boolean with Singleton
  val value: Out {}
  val valueWide: OutWide
}

trait Repeater[P <: SingletonTypeExpr] extends SingletonTypeExpr

object Repeater {
  type Aux[P <: SingletonTypeExpr,
           Ret_OutWide,
           Ret_Out] =
    Repeater[P] { type OutWide = Ret_OutWide; type Out = Ret_Out }

  implicit def impl[P <: SingletonTypeExpr,
                    Ret_OutWide,
                    Ret_Out](
      implicit p: P): Aux[P, p.OutWide, p.Out] = new Repeater[P] {
    type OutWide = p.OutWide; type Out = p.Out; val value: Out = p.value; val valueWide : OutWide = p.valueWide
  }
}

trait SingletonTypeValue[S] extends SingletonTypeExpr

object SingletonTypeValue {

  implicit def implExpr[P1 <: SingletonTypeExpr](
      implicit op: Repeater[P1]): SingletonTypeValue[P1] {
    type OutWide = op.OutWide; type Out = op.Out
  } =
    new SingletonTypeValue[P1] {
      type OutWide = op.OutWide; type Out = op.Out; val value: Out = op.value; val valueWide : OutWide = op.valueWide
    }

  implicit def impl[S]
  (implicit id: Op1Macro["Id", S]): SingletonTypeValue[S] {
    type OutWide = id.OutWide; type Out = id.Out
  } = new SingletonTypeValue[S] {
    type OutWide = id.OutWide; type Out = id.Out; val value: Out = id.value;  val valueWide : OutWide = id.valueWide
  }
}

trait SingletonTypeFunc1[N <: String with Singleton, P1 <: SingletonTypeExpr]
    extends SingletonTypeExpr

object SingletonTypeFunc1 {
  type Aux[
      N <: String with Singleton,
      P1 <: SingletonTypeExpr,
      Ret_OutWide,
      Ret_Out
  ] = SingletonTypeFunc1[N, P1] {
    type OutWide = Ret_OutWide
    type Out = Ret_Out
  }

  implicit def impl[
      N <: String with Singleton,
      P1 <: SingletonTypeExpr,
      P1_OutWide,
      P1_Out,
      Ret_OutWide,
      Ret_Out
  ](implicit p1_ret: Repeater.Aux[P1, P1_OutWide, P1_Out],
    op: Op1Macro[N, P1_Out])
    : Aux[N, P1, op.OutWide, op.Out] {
    type OutNat = op.OutNat
    type OutInt = op.OutInt
    type OutLong = op.OutLong
    type OutDouble = op.OutDouble
    type OutString = op.OutString
    type OutBoolean = op.OutBoolean
  }=
    new SingletonTypeFunc1[N, P1] {
      type OutWide = op.OutWide
      type Out = op.Out
      type OutNat = op.OutNat
      type OutInt = op.OutInt
      type OutLong = op.OutLong
      type OutDouble = op.OutDouble
      type OutString = op.OutString
      type OutBoolean = op.OutBoolean
      val value: Out {} = op.value
      val valueWide : OutWide = op.valueWide
    }
}

trait SingletonTypeFunc2[N <: String with Singleton,
                         P1 <: SingletonTypeExpr,
                         P2 <: SingletonTypeExpr]
    extends SingletonTypeExpr

object SingletonTypeFunc2 {
  type Aux[
      N <: String with Singleton,
      P1 <: SingletonTypeExpr,
      P2 <: SingletonTypeExpr,
      Ret_OutWide,
      Ret_Out
  ] = SingletonTypeFunc2[N, P1, P2] {
    type OutWide = Ret_OutWide
    type Out = Ret_Out
  }

  implicit def impl[
      N <: String with Singleton,
      P1 <: SingletonTypeExpr,
      P2 <: SingletonTypeExpr,
      P1_OutWide,
      P1_Out,
      P2_OutWide,
      P2_Out,
      Ret_OutWide,
      Ret_Out
  ](implicit p1_ret: Repeater.Aux[P1, P1_OutWide, P1_Out],
    p2_ret: Repeater.Aux[P2, P2_OutWide, P2_Out],
    op: Op2Macro[N, P1_Out, P2_Out])
    : Aux[N, P1, P2, op.OutWide, op.Out] {
      type OutNat = op.OutNat
      type OutInt = op.OutInt
      type OutLong = op.OutLong
      type OutDouble = op.OutDouble
      type OutString = op.OutString
      type OutBoolean = op.OutBoolean
     }  =
    new SingletonTypeFunc2[N, P1, P2] {
      type Out = op.Out
      type OutWide = op.OutWide
      type OutNat = op.OutNat
      type OutInt = op.OutInt
      type OutLong = op.OutLong
      type OutDouble = op.OutDouble
      type OutString = op.OutString
      type OutBoolean = op.OutBoolean
      val value: Out {} = op.value
      val valueWide : OutWide = op.valueWide
    }
}

