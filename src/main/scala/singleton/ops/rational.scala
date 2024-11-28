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
      LN, LD,
      RN, RD,
      LNRD, RNLD,
      N, D,
      SN, SD](
    implicit
      rat: Require[IsRational[LHS] || IsRational[RHS]],
      lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
      rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
      ev0: OpAuxGen[LN * RD, LNRD],
      ev1: OpAuxGen[RN * LD, RNLD],
      ev2: OpAuxGen[LNRD + RNLD, N],
      ev3: OpAuxGen[LD * RD, D],
      ev4: OpAuxGen[Simplify[Rational[N, D]], Rational[SN, SD]],
    ): OpIntercept.Aux[LHS + RHS, Rational[SN, SD]] =
  new OpIntercept[LHS + RHS] {
    type Out = Rational[SN, SD]
    val value: Out = new Rational[SN, SD] {}
  }

  implicit def doRationalSubtract[
      LHS, RHS,
      LN, LD,
      RN, RD, RNN,
      SN, SD](
    implicit
      rat: Require[IsRational[LHS] || IsRational[RHS]],
      lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
      rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
      neg: OpAuxGen[Negate[RN], RNN],
      add: OpAuxGen[Rational[LN, LD] + Rational[RNN, RD], Rational[SN, SD]]
    ): OpIntercept.Aux[LHS - RHS, Rational[SN, SD]] =
  new OpIntercept[LHS - RHS] {
    type Out = Rational[SN, SD]
    val value: Out = new Rational[SN, SD] {}
  }

  implicit def doRationalMultiply[
      LHS, RHS,
      LN, LD,
      RN, RD,
      N, D,
      SN, SD](
    implicit
      rat: Require[IsRational[LHS] || IsRational[RHS]],
      lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
      rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
      ev0: OpAuxGen[LN * RN, N],
      ev1: OpAuxGen[LD * RD, D],
      ev2: OpAuxGen[Simplify[Rational[N, D]], Rational[SN, SD]]
    ): OpIntercept.Aux[LHS * RHS, Rational[SN, SD]] =
    new OpIntercept[LHS * RHS] {
      type Out = Rational[SN, SD]
      val value: Out = new Rational[SN, SD] {}
    }

  implicit def doRationalDivide[
      LHS, RHS,
      LN, LD,
      RN, RD,
      SN, SD](
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

  implicit def doSimplifyPositive[N, D, C, SN, SD](implicit
      pos: RequireMsg[IsPositive[N] && IsPositive[D], simplifyErrorMsg],
      gcd: OpAuxGen[GCD[N, D], C],
      n: OpAuxGen[N / C, SN],
      d: OpAuxGen[D / C, SD]
    ): OpIntercept.Aux[Simplify[Rational[N, D]], Rational[SN, SD]] =
    new OpIntercept[Simplify[Rational[N, D]]] {
      type Out = Rational[SN, SD]
      val value = new Rational[SN, SD] {}
    }

  implicit def doSimplifyNegative[N, D, F, SNF, SN, SD](implicit
      neg: RequireMsg[IsNegative[N] && IsPositive[D], simplifyErrorMsg],
      ev1: OpAuxGen[Negate[Rational[N, D]], F],
      ev2: OpAuxGen[Simplify[F], SNF],
      ev3: OpAuxGen[Negate[SNF], Rational[SN, SD]]
    ): OpIntercept.Aux[Simplify[Rational[N, D]], Rational[SN, SD]] =
    new OpIntercept[Simplify[Rational[N, D]]] {
      type Out = Rational[SN, SD]
      val value = new Rational[SN, SD] {}
    }

  implicit def doSimplifyZero[Z, D](implicit
      zro: RequireMsg[IsZero[Z] && IsPositive[D], simplifyErrorMsg],
      v1: AlignType[1, D]
    ): OpIntercept.Aux[Simplify[Rational[Z, D]], Rational[Z, v1.Out]] =
    new OpIntercept[Simplify[Rational[Z, D]]] {
      type Out = Rational[Z, v1.Out]
      val value = new Rational[Z, v1.Out] {}
    }

  implicit def doSimplifyNegDenom[N, D, NN, ND, SN, SD](implicit
      neg: RequireMsg[IsNegative[D], simplifyErrorMsg],
      nn: OpAuxGen[Negate[N], NN],
      nd: OpAuxGen[Negate[D], ND],
      sf: OpAuxGen[Simplify[Rational[NN, ND]], Rational[SN, SD]]
    ): OpIntercept.Aux[Simplify[Rational[N, D]], Rational[SN, SD]] =
    new OpIntercept[Simplify[Rational[N, D]]] {
      type Out = Rational[SN, SD]
      val value = new Rational[SN, SD] {}
    }
}
