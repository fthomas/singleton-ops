package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class OpInterceptSpec extends Properties("OpInterceptSpec") {

  trait Vec[A0, A1]

  implicit def `Vec+`[VL0, VL1, VR0, VR1, VO0, VO1](
    implicit
    opL : OpAuxGen[VL0 + VR0, VO0],
    opR : OpAuxGen[VL1 + VR1, VO1],
    result : OpIntercept.CacheResult[Vec[VO0, VO1]]
  ) : OpIntercept[Vec[VL0, VL1] + Vec[VR0, VR1]] = ???

  implicit def `Vec==`[VL0, VL1, VR0, VR1, EqOut](
    implicit
    op : OpAuxGen[(VL0 == VR0) && (VL1 == VR1), EqOut],
    result : OpIntercept.CacheResult[EqOut]
  ) : OpIntercept[Vec[VL0, VL1] == Vec[VR0, VR1]] = ???


  property("Custom Vec Equality OK") = wellTyped {
    val eq1 = shapeless.the[Vec[1, 2] == Vec[1, 2]]
    val eq2 = shapeless.the[Vec[1, 2] == Vec[1, 1]]
    implicitly[eq1.Out =:= true]
    implicitly[eq2.Out =:= false]
  }

  property("Custom Vec Addition OK") = wellTyped {
    val add2 = shapeless.the[Vec[1, 2] + Vec[3, 8]]
    val add3 = shapeless.the[Vec[1, 2] + Vec[3, 8] + Vec[20, 20]]
    implicitly[add2.Out =:= Vec[4, 10]]
    implicitly[add3.Out =:= Vec[24, 30]]
    val add23 = shapeless.the[add2.Out + add3.Out]
    implicitly[add23.Out =:= Vec[28, 40]]
  }

  trait FibId
  type Fib[P] = impl.OpMacro[FibId, P, 0, 0]
  implicit def doFib[P, Out](
    implicit
    op : OpAuxGen[ITE[P == 0, 0, ITE[P == 1, 1, Fib[P - 1] + Fib[P - 2]]], Out],
    result : OpIntercept.CacheResult[Out]
  ) : OpIntercept[Fib[P]] = ???


  property("Custom Fibonacci Op OK") = wellTyped {
    val fib4 = shapeless.the[Fib[4]]
    implicitly[fib4.Out =:= 3]
    val fib10 = shapeless.the[Fib[10]]
    implicitly[fib10.Out =:= 55]
  }
}
