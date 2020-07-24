package singleton.ops

import singleton.ops.impl.OpMacro

object Math {
  trait NumericLiteral[T, UB]
  implicit def __numericIsNumeric[T, UB]: OpInterceptAux[NumericLiteral[T, UB] => NumericLiteral[_,_], NumericLiteral[T, UB]] = ???
  implicit def __numericInt[T <: XInt] : OpInterceptAux[T => NumericLiteral[_,_], NumericLiteral[T, XInt]] = ???
  implicit def __numericLong[T <: XLong] : OpInterceptAux[T => NumericLiteral[_,_], NumericLiteral[T, XLong]] = ???
  implicit def __numericFloat[T <: XFloat] : OpInterceptAux[T => NumericLiteral[_,_], NumericLiteral[T, XFloat]] = ???
  implicit def __numericDouble[T <: XDouble] : OpInterceptAux[T => NumericLiteral[_,_], NumericLiteral[T, XDouble]] = ???

  trait IntegralLiteral[T, UB] extends NumericLiteral[T, UB]
  implicit def __integralInt[T] : OpInterceptAux[NumericLiteral[T, XInt] => IntegralLiteral[_,_], IntegralLiteral[T, XInt]] = ???
  implicit def __integralLong[T] : OpInterceptAux[NumericLiteral[T, XLong] => IntegralLiteral[_,_], IntegralLiteral[T, XLong]] = ???

  trait Zero
  implicit def __zeroInt : OpInterceptAux[Zero => XInt, W.`0`.T] = ???
  implicit def __zeroLong : OpInterceptAux[Zero => XLong, W.`0L`.T] = ???
  implicit def __zeroFloat : OpInterceptAux[Zero => XFloat, W.`0.0f`.T] = ???
  implicit def __zeroDouble : OpInterceptAux[Zero => XDouble, W.`0.0`.T] = ???

//  implicit def `__numeric+`[L, NL[_,_], TL, R, NR[_,_], TR, UB](
//    implicit
//    numericL : OpInterceptAux[L => NumericLiteral[_,_], NL[TL, UB]],
//    numericR : OpInterceptAux[R => NumericLiteral[_,_], NR[TR, UB]],
//    op : TL + TR
//  ) : OpInterceptAux[L + R, NumericLiteral[op.Out, UB]] = ???
//
//  implicit def `__numeric-`[L, NL[_,_], TL, R, NR[_,_], TR, UB](
//    implicit
//    numericL : OpInterceptAux[L => NumericLiteral[_,_], NL[TL, UB]],
//    numericR : OpInterceptAux[R => NumericLiteral[_,_], NR[TR, UB]],
//    op : TL - TR
//  ) : OpInterceptAux[L - R, NumericLiteral[op.Out, UB]] = ???
//
//  implicit def `__numeric*`[L, NL[_,_], TL, R, NR[_,_], TR, UB](
//    implicit
//    numericL : OpInterceptAux[L => NumericLiteral[_,_], NL[TL, UB]],
//    numericR : OpInterceptAux[R => NumericLiteral[_,_], NR[TR, UB]],
//    op : TL * TR
//  ) : OpInterceptAux[L * R, NumericLiteral[op.Out, UB]] = ???
//
//  implicit def `__numeric/`[L, NL[_,_], TL, R, NR[_,_], TR, UB](
//    implicit
//    numericL : OpInterceptAux[L => IntegralLiteral[_,_], NL[TL, UB]],
//    numericR : OpInterceptAux[R => IntegralLiteral[_,_], NR[TR, UB]],
//    op : TL / TR
//  ) : OpInterceptAux[L / R, IntegralLiteral[op.Out, UB]] = ???
//
//  implicit def `__numeric%`[L, NL[_,_], TL, R, NR[_,_], TR, UB](
//    implicit
//    numericL : OpInterceptAux[L => IntegralLiteral[_,_], NL[TL, UB]],
//    numericR : OpInterceptAux[R => IntegralLiteral[_,_], NR[TR, UB]],
//    op : TL % TR
//  ) : OpInterceptAux[L % R, IntegralLiteral[op.Out, UB]] = ???


//  trait IsPositiveLiteral[T]
//  implicit def __isPositive[T](
//    implicit
//    positive : T > ZeroLiteralOf[T]
//  ) : OpInterceptAux[IsPositiveLiteral[T], positive.Out] = ???
//
//  trait PositiveLiteral[T] extends NumericLiteral[T]
//  implicit def __positiveNumeric[T](
//    implicit
//    positive : RequireMsg[IsPositiveLiteral[T], W.`"Must be a positive number, but found "`.T + ToString[T]]
//  ) : OpInterceptAux[NumericLiteral[T] => PositiveLiteral[_], PositiveLiteral[T]] = ???



//  trait IsNegativeOpId
//  type IsNegative[P] = OpMacro[IsNegativeOpId, P, W.`0`.T, W.`0`.T]
//  implicit def doIsNegative[P](implicit
//    tst: P < AlignType[0, P]): OpInterceptAux[IsNegative[P], tst.Out] = ???
//
//  trait IsZeroOpId
//  type IsZero[P] = OpMacro[IsZeroOpId, P, W.`0`.T, W.`0`.T]
//  implicit def doIsZero[P](implicit
//    tst: P == AlignType[0, P]): OpInterceptAux[IsZero[P], tst.Out] = ???
//
//  trait IsNonZeroOpId
//  type IsNonZero[P] = OpMacro[IsNonZeroOpId, P, W.`0`.T, W.`0`.T]
//  implicit def doIsNonZero[P](implicit
//    tst: P != AlignType[0, P]): OpInterceptAux[IsNonZero[P], tst.Out] = ???



}
