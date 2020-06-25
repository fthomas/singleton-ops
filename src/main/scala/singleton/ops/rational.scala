package singleton.ops

import singleton.ops.impl.OpMacro

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

  implicit def __rational2rational[N, D] : OpInterceptAux[Rational[N, D] ==> Rational[_,_], Rational[N, D]] = ???
  implicit def __int2rational[I <: XInt] : OpInterceptAux[I ==> Rational[_,_], Rational[I, W.`1`.T]] = ???
  implicit def __long2rational[L <: XLong] : OpInterceptAux[L ==> Rational[_,_], Rational[L, W.`1L`.T]] = ???

  implicit def __negateRational[N, D](implicit negateN : Negate[N])
  : OpInterceptAux[Negate[Rational[N, D]], Rational[negateN.Out, D]] = ???

  implicit def `__rational+`[L, LN, LD, LRat[_,_], R, RN, RD, RRat[_,_], ON, OD] (
    implicit
    rationalL : OpInterceptAux[L ==> Rational[_,_], LRat[LN, LD]],
    rationalR : OpInterceptAux[R ==> Rational[_,_], RRat[RN, RD]],
    nom : OpGenAux[(LN * RD) + (RN * LD), ON],
    den : OpGenAux[LD * RD, OD]
  ) : OpInterceptAux[L + R, Rational[nom.Out, den.Out]] = ???


  implicitly[Rational[1, 2] + 1]

  //
//  implicit def doRationalSubtract[
//    LHS, RHS,
//    LN, LD,
//    RN, RD, RNN,
//    SN, SD](
//    implicit
//    rat: Require[IsRational[LHS] || IsRational[RHS]],
//    lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
//    rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
//    neg: OpAuxGen[Negate[RN], RNN],
//    add: OpAuxGen[Rational[LN, LD] + Rational[RNN, RD], Rational[SN, SD]]
//  ): OpInterceptAux[LHS - RHS, Rational[SN, SD]] =
//    new OpIntercept[LHS - RHS] {
//      type Out = Rational[SN, SD]
//      val value: Out = new Rational[SN, SD] {}
//    }
//
//  implicit def doRationalMultiply[
//    LHS, RHS,
//    LN, LD,
//    RN, RD,
//    N, D,
//    SN, SD](
//    implicit
//    rat: Require[IsRational[LHS] || IsRational[RHS]],
//    lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
//    rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
//    ev0: OpAuxGen[LN * RN, N],
//    ev1: OpAuxGen[LD * RD, D],
//    ev2: OpAuxGen[Simplify[Rational[N, D]], Rational[SN, SD]]
//  ): OpInterceptAux[LHS * RHS, Rational[SN, SD]] =
//    new OpIntercept[LHS * RHS] {
//      type Out = Rational[SN, SD]
//      val value: Out = new Rational[SN, SD] {}
//    }
//
//  implicit def doRationalDivide[
//    LHS, RHS,
//    LN, LD,
//    RN, RD,
//    SN, SD](
//    implicit
//    rat: Require[IsRational[LHS] || IsRational[RHS]],
//    lhs: OpAuxGen[ToRational[LHS], Rational[LN, LD]],
//    rhs: OpAuxGen[ToRational[RHS], Rational[RN, RD]],
//    mul: OpAuxGen[Rational[LN, LD] * Rational[RD, RN], Rational[SN, SD]]
//  ): OpInterceptAux[LHS / RHS, Rational[SN, SD]] =
//    new OpIntercept[LHS / RHS] {
//      type Out = Rational[SN, SD]
//      val value: Out = new Rational[SN, SD] {}
//    }
//
//  trait GCDOpId
//  type GCD[A, B] = OpMacro[GCDOpId, A, B, W.`0`.T]
//
//  private type gcdErrorMsg = W.`"GCD requires positive integral arguments"`.T
//
//  implicit def doGCDforBasisCase[A, B, Rem](implicit
//    ev0: RequireMsg[IsIntegral[A] && IsIntegral[B] && (A >= B) && IsPositive[B], gcdErrorMsg],
//    ev1: OpAuxGen[A % B, Rem],
//    ev2: Require[IsZero[Rem]]): OpInterceptAux[GCD[A, B], B] = ???
//
//  implicit def doGCDforAgeB[A, B, Rem, D](implicit
//    ev0: RequireMsg[IsIntegral[A] && IsIntegral[B] && (A >= B) && IsPositive[B], gcdErrorMsg],
//    ev1: OpAuxGen[A % B, Rem],
//    ev2: Require[IsNonZero[Rem]],
//    ev3: OpAuxGen[GCD[B, Rem], D]): OpInterceptAux[GCD[A, B], D] = ???
//
//  implicit def doGCDforAltB[A, B, Rem, D](implicit
//    ev0: RequireMsg[IsIntegral[A] && IsIntegral[B] && (A < B) && IsPositive[A], gcdErrorMsg],
//    ev1: OpAuxGen[GCD[B, A], D]): OpInterceptAux[GCD[A, B], D] = ???
//
//  trait SimplifyOpId
//  type Simplify[F] = OpMacro[SimplifyOpId, F, W.`0`.T, W.`0`.T]
//
//  private type simplifyErrorMsg = W.`"Simplify requires non-zero denominator"`.T
//
//  implicit def doSimplifyPositive[N, D, C, SN, SD](implicit
//    pos: RequireMsg[IsPositive[N] && IsPositive[D], simplifyErrorMsg],
//    gcd: OpAuxGen[GCD[N, D], C],
//    n: OpAuxGen[N / C, SN],
//    d: OpAuxGen[D / C, SD]
//  ): OpInterceptAux[Simplify[Rational[N, D]], Rational[SN, SD]] =
//    new OpIntercept[Simplify[Rational[N, D]]] {
//      type Out = Rational[SN, SD]
//      val value = new Rational[SN, SD] {}
//    }
//
//  implicit def doSimplifyNegative[N, D, F, SNF, SN, SD](implicit
//    neg: RequireMsg[IsNegative[N] && IsPositive[D], simplifyErrorMsg],
//    ev1: OpAuxGen[Negate[Rational[N, D]], F],
//    ev2: OpAuxGen[Simplify[F], SNF],
//    ev3: OpAuxGen[Negate[SNF], Rational[SN, SD]]
//  ): OpInterceptAux[Simplify[Rational[N, D]], Rational[SN, SD]] =
//    new OpIntercept[Simplify[Rational[N, D]]] {
//      type Out = Rational[SN, SD]
//      val value = new Rational[SN, SD] {}
//    }
//
//  implicit def doSimplifyZero[Z, D](implicit
//    zro: RequireMsg[IsZero[Z] && IsPositive[D], simplifyErrorMsg],
//    v1: AlignType[1, D]
//  ): OpInterceptAux[Simplify[Rational[Z, D]], Rational[Z, v1.Out]] =
//    new OpIntercept[Simplify[Rational[Z, D]]] {
//      type Out = Rational[Z, v1.Out]
//      val value = new Rational[Z, v1.Out] {}
//    }
//
//  implicit def doSimplifyNegDenom[N, D, NN, ND, SN, SD](implicit
//    neg: RequireMsg[IsNegative[D], simplifyErrorMsg],
//    nn: OpAuxGen[Negate[N], NN],
//    nd: OpAuxGen[Negate[D], ND],
//    sf: OpAuxGen[Simplify[Rational[NN, ND]], Rational[SN, SD]]
//  ): OpInterceptAux[Simplify[Rational[N, D]], Rational[SN, SD]] =
//    new OpIntercept[Simplify[Rational[N, D]]] {
//      type Out = Rational[SN, SD]
//      val value = new Rational[SN, SD] {}
//    }
}
