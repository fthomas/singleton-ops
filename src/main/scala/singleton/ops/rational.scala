package singleton.ops

import singleton.ops._
import singleton.ops.impl.{OpCast, OpGen, OpInt, OpMacro}

object rational {
  /** Represents a rational number
    *
    * @tparam N the numerator
    * @tparam D the denominator
    */
  trait Rational[N, D] {
    // currently only XInt is supported,
    // other types such as XLong could be added with additional implicit rules
    def n(implicit nv: Id[N]): nv.Out = nv.value
    def d(implicit dv: Id[D]): dv.Out = dv.value
    def show(implicit nv: Id[N], dv: Id[D]): String = s"Rational(${n}, ${d})"
  }

  trait AlignTypeOpId
  type AlignType[V, P] = OpMacro[AlignTypeOpId, V, P, W.`0`.T]
  implicit def doAlignTypeInt[V, P <: XInt](implicit
      v: ToInt[V]): OpIntercept.Aux[AlignType[V, P], v.Out] = ???
  implicit def doAlignTypeLong[V, P <: XLong](implicit
      v: ToLong[V]): OpIntercept.Aux[AlignType[V, P], v.Out] = ???
  implicit def doAlignTypeFloat[V, P <: XFloat](implicit
      v: ToFloat[V]): OpIntercept.Aux[AlignType[V, P], v.Out] = ???
  implicit def doAlignTypeDouble[V, P <: XDouble](implicit
      v: ToDouble[V]): OpIntercept.Aux[AlignType[V, P], v.Out] = ???

  trait IsPositiveOpId
  type IsPositive[P] = OpMacro[IsPositiveOpId, P, W.`0`.T, W.`0`.T]
  implicit def doIsPositive[P](implicit
      tst: P > AlignType[0, P]): OpIntercept.Aux[IsPositive[P], tst.Out] = ???

  trait IsNegativeOpId
  type IsNegative[P] = OpMacro[IsNegativeOpId, P, W.`0`.T, W.`0`.T]
  implicit def doIsNegative[P](implicit
      tst: P < AlignType[0, P]): OpIntercept.Aux[IsNegative[P], tst.Out] = ???

  trait IsZeroOpId
  type IsZero[P] = OpMacro[IsZeroOpId, P, W.`0`.T, W.`0`.T]
  implicit def doIsZero[P](implicit
      tst: P == AlignType[0, P]): OpIntercept.Aux[IsZero[P], tst.Out] = ???

  trait IsNonZeroOpId
  type IsNonZero[P] = OpMacro[IsNonZeroOpId, P, W.`0`.T, W.`0`.T]
  implicit def doIsNonZero[P](implicit
      tst: P != AlignType[0, P]): OpIntercept.Aux[IsNonZero[P], tst.Out] = ???

  trait IsIntegralOpId
  type IsIntegral[P] = OpMacro[IsIntegralOpId, P, W.`0`.T, W.`0`.T]
  implicit def doIsIntegral[P](implicit
      tst: IsInt[P] || IsLong[P]): OpIntercept.Aux[IsIntegral[P], tst.Out] = ???

  private trait IsRationalImpl[P] {
    type Out
  }
  private trait IsRationalImplDefault {
    type Aux[P, O] = IsRationalImpl[P] { type Out = O }
    implicit def isRationalFalse[P]: Aux[P, false] =
      new IsRationalImpl[P] {
        type Out = false
      }
  }
  private object IsRationalImpl extends IsRationalImplDefault {
    implicit def isRationalTrue[N, D]: Aux[Rational[N, D], true] =
      new IsRationalImpl[Rational[N, D]] {
        type Out = true
      }    
  }

  trait IsRationalOpId
  type IsRational[P] = OpMacro[IsRationalOpId, P, W.`0`.T, W.`0`.T]

  implicit def doIsRational[P, T](implicit
      tst: IsRationalImpl.Aux[P, T]): OpIntercept.Aux[IsRational[P], T] = ???

  trait ToRationalOpId
  type ToRational[P] = OpMacro[ToRationalOpId, P, W.`0`.T, W.`0`.T]

  implicit def toRationalFromRat[
      N <: XInt, D <: XInt,
      SN <: XInt, SD <: XInt](
    implicit
      sim: OpAuxGen[Simplify[Rational[N, D]], Rational[SN, SD]]
    ): OpIntercept.Aux[ToRational[Rational[N, D]], Rational[SN, SD]] =
    new OpIntercept[ToRational[Rational[N, D]]] {
      type Out = Rational[SN, SD]
      val value: Out = new Rational[SN, SD] {}
    }

  implicit def toRationalFromInt[N, One](implicit
      one: OpAuxGen[AlignType[1, N], One]): OpIntercept.Aux[ToRational[N], Rational[N, One]] =
    new OpIntercept[ToRational[N]] {
      type Out = Rational[N, One]
      val value: Out = new Rational[N, One] {}
    }

  implicit def doRationalNegate[N, D, NN](implicit
      neg: OpAuxGen[Negate[N], NN]): OpIntercept.Aux[Negate[Rational[N, D]], Rational[NN, D]] =
    new OpIntercept[Negate[Rational[N, D]]] {
      type Out = Rational[NN, D]
      val value: Out = new Rational[NN, D] {}
    }

  implicit def doRationalAdd[
      LHS, RHS,
      LN <: XInt, LD <: XInt,
      RN <: XInt, RD <: XInt,
      LNRD <: XInt, RNLD <: XInt,
      N <: XInt, D <: XInt,
      SN <: XInt, SD <: XInt](
    implicit
      rat: Require[IsRational[LHS] || IsRational[RHS]],
      lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
      rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
      ev0: OpInt.Aux[LN * RD, LNRD],
      ev1: OpInt.Aux[RN * LD, RNLD],
      ev2: OpInt.Aux[LNRD + RNLD, N],
      ev3: OpInt.Aux[LD * RD, D],
      ev4: OpAuxGen[Simplify[Rational[N, D]], Rational[SN, SD]],
    ): OpIntercept.Aux[LHS + RHS, Rational[SN, SD]] =
  new OpIntercept[LHS + RHS] {
    type Out = Rational[SN, SD]
    val value: Out = new Rational[SN, SD] {}
  }

  implicit def doRationalSubtract[
      LHS, RHS,
      LN <: XInt, LD <: XInt,
      RN <: XInt, RD <: XInt, RNN <: XInt,
      SN <: XInt, SD <: XInt](
    implicit
      rat: Require[IsRational[LHS] || IsRational[RHS]],
      lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
      rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
      neg: OpInt.Aux[Negate[RN], RNN],
      add: OpAuxGen[Rational[LN, LD] + Rational[RNN, RD], Rational[SN, SD]]
    ): OpIntercept.Aux[LHS - RHS, Rational[SN, SD]] =
  new OpIntercept[LHS - RHS] {
    type Out = Rational[SN, SD]
    val value: Out = new Rational[SN, SD] {}
  }

  implicit def doRationalMultiply[
      LHS, RHS,
      LN <: XInt, LD <: XInt,
      RN <: XInt, RD <: XInt,
      N <: XInt, D <: XInt,
      SN <: XInt, SD <: XInt](
    implicit
      rat: Require[IsRational[LHS] || IsRational[RHS]],
      lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
      rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
      ev0: OpInt.Aux[LN * RN, N],
      ev1: OpInt.Aux[LD * RD, D],
      ev2: OpAuxGen[Simplify[Rational[N, D]], Rational[SN, SD]]
    ): OpIntercept.Aux[LHS * RHS, Rational[SN, SD]] =
    new OpIntercept[LHS * RHS] {
      type Out = Rational[SN, SD]
      val value: Out = new Rational[SN, SD] {}
    }

  implicit def doRationalDivide[
      LHS, RHS,
      LN <: XInt, LD <: XInt,
      RN <: XInt, RD <: XInt,
      SN <: XInt, SD <: XInt](
    implicit
      rat: Require[IsRational[LHS] || IsRational[RHS]],
      lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
      rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
      mul: OpAuxGen[Rational[LN, LD] * Rational[RD, RN], Rational[SN, SD]]
    ): OpIntercept.Aux[LHS / RHS, Rational[SN, SD]] =
    new OpIntercept[LHS / RHS] {
      type Out = Rational[SN, SD]
      val value: Out = new Rational[SN, SD] {}
    }

  trait GCDOpId
  type GCD[A, B] = OpMacro[GCDOpId, A, B, W.`0`.T]

  private type gcdErrorMsg = W.`"GCD requires positive integral arguments"`.T

  implicit def doGCDforBasisCase[A, B, Rem](implicit
      ev0: RequireMsg[IsIntegral[A] && IsIntegral[B] && (A >= B) && IsPositive[B], gcdErrorMsg],
      ev1: OpAuxGen[A % B, Rem],
      ev2: Require[IsZero[Rem]]): OpIntercept.Aux[GCD[A, B], B] = ???

  implicit def doGCDforAgeB[A, B, Rem, D](implicit
      ev0: RequireMsg[IsIntegral[A] && IsIntegral[B] && (A >= B) && IsPositive[B], gcdErrorMsg],
      ev1: OpAuxGen[A % B, Rem],
      ev2: Require[IsNonZero[Rem]],
      ev3: OpAuxGen[GCD[B, Rem], D]): OpIntercept.Aux[GCD[A, B], D] = ???

  implicit def doGCDforAltB[A, B, Rem, D](implicit
      ev0: RequireMsg[IsIntegral[A] && IsIntegral[B] && (A < B) && IsPositive[A], gcdErrorMsg],
      ev1: OpAuxGen[GCD[B, A], D]): OpIntercept.Aux[GCD[A, B], D] = ???

  trait SimplifyOpId
  type Simplify[F] = OpMacro[SimplifyOpId, F, W.`0`.T, W.`0`.T]

  private type simplifyErrorMsg = W.`"Simplify requires non-zero denominator"`.T

  implicit def doSimplifyPositive[
      N <: XInt, D <: XInt,
      C <: XInt,
      SN <: XInt, SD <: XInt](
    implicit
      ev0: RequireMsg[(N > W.`0`.T) && (D > W.`0`.T), simplifyErrorMsg],
      gcd: OpInt.Aux[GCD[N, D], C],
      n: OpInt.Aux[N / C, SN],
      d: OpInt.Aux[D / C, SD]
    ): OpIntercept.Aux[Simplify[Rational[N, D]], Rational[SN, SD]] =
    new OpIntercept[Simplify[Rational[N, D]]] {
      type Out = Rational[SN, SD]
      val value = new Rational[SN, SD] {}
    }

  implicit def doSimplifyNegative[
      N <: XInt, D <: XInt,
      F <: Rational[_, _],
      SNF <: Rational[_, _],
      SN <: XInt,  SD <: XInt](
    implicit
      ev0: RequireMsg[(N < W.`0`.T) && (D > W.`0`.T), simplifyErrorMsg],
      ev1: OpAuxGen[Negate[Rational[N, D]], F],
      ev2: OpAuxGen[Simplify[F], SNF],
      ev3: OpAuxGen[Negate[SNF], Rational[SN, SD]]
    ): OpIntercept.Aux[Simplify[Rational[N, D]], Rational[SN, SD]] =
    new OpIntercept[Simplify[Rational[N, D]]] {
      type Out = Rational[SN, SD]
      val value = new Rational[SN, SD] {}
    }

  implicit def doSimplifyZero[D <: XInt](implicit
      nz: RequireMsg[D > W.`0`.T, simplifyErrorMsg]
    ): OpIntercept.Aux[Simplify[Rational[W.`0`.T, D]], Rational[W.`0`.T, W.`1`.T]] =
    new OpIntercept[Simplify[Rational[W.`0`.T, D]]] {
      type Out = Rational[W.`0`.T, W.`1`.T]
      val value = new Rational[W.`0`.T, W.`1`.T] {}
    }

  implicit def doSimplifyNegDenom[
      N <: XInt, D <: XInt,
      NN <: XInt, ND <: XInt,
      SN <: XInt, SD <: XInt](
    implicit
      bn: RequireMsg[D < W.`0`.T, simplifyErrorMsg],
      nn: OpInt.Aux[Negate[N], NN],
      nd: OpInt.Aux[Negate[D], ND],
      sf: OpAuxGen[Simplify[Rational[NN, ND]], Rational[SN, SD]]
    ): OpIntercept.Aux[Simplify[Rational[N, D]], Rational[SN, SD]] =
    new OpIntercept[Simplify[Rational[N, D]]] {
      type Out = Rational[SN, SD]
      val value = new Rational[SN, SD] {}
    }
}
