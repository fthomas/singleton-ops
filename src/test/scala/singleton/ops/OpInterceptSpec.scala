package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
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
    val eq1 = shapeless.the[Vec[W.`1`.T, W.`2`.T] == Vec[W.`1`.T, W.`2`.T]]
    val eq2 = shapeless.the[Vec[W.`1`.T, W.`2`.T] == Vec[W.`1`.T, W.`1`.T]]
    implicitly[eq1.Out =:= W.`true`.T]
    implicitly[eq2.Out =:= W.`false`.T]
  }

  property("Custom Vec Addition OK") = wellTyped {
    val add2 = shapeless.the[Vec[W.`1`.T, W.`2`.T] + Vec[W.`3`.T, W.`8`.T]]
    val add3 = shapeless.the[Vec[W.`1`.T, W.`2`.T] + Vec[W.`3`.T, W.`8`.T] + Vec[W.`20`.T, W.`20`.T]]
    implicitly[add2.Out =:= Vec[W.`4`.T, W.`10`.T]]
    implicitly[add3.Out =:= Vec[W.`24`.T, W.`30`.T]]
    val add23 = shapeless.the[add2.Out + add3.Out]
    implicitly[add23.Out =:= Vec[W.`28`.T, W.`40`.T]]
  }

  trait FibId
  type Fib[P] = impl.OpMacro[FibId, P, W.`0`.T, W.`0`.T]
  implicit def doFib[P, Out](
    implicit
    op : OpAuxGen[ITE[P == W.`0`.T, W.`0`.T, ITE[P == W.`1`.T, W.`1`.T, Fib[P - W.`1`.T] + Fib[P - W.`2`.T]]], Out],
    result : OpIntercept.CacheResult[Out]
  ) : OpIntercept[Fib[P]] = ???


  property("Custom Fibonacci Op OK") = wellTyped {
    val fib4 = shapeless.the[Fib[W.`4`.T]]
    implicitly[fib4.Out =:= W.`3`.T]
    val fib10 = shapeless.the[Fib[W.`10`.T]]
    implicitly[fib10.Out =:= W.`55`.T]
  }


  trait FooOpId
  type FooOp[C, M] = impl.OpMacro[FooOpId, C, M, W.`0`.T]
  implicit def FooOp[C, M](
    implicit
    r : RequireMsg[C, M],
    result : OpIntercept.CacheResult[W.`true`.T]
  ) : OpIntercept[FooOp[C, M]] = ???

  property("Error Message Propagation") = wellTyped {
    illTyped("""shapeless.the[FooOp[W.`false`.T, W.`"this is a test"`.T]]""", "this is a test")
  }

  trait BarOpId
  type BarOp[C, M] = impl.OpMacro[BarOpId, C, M, W.`0`.T]
  implicit def BarOp[C, M](
    implicit
    op : C + M
  ) : OpIntercept[BarOp[C, M]] = ???

  property("Missing Caching Error") = wellTyped {
    illTyped("""shapeless.the[BarOp[W.`1`.T, W.`2`.T]]""", "Missing a result cache for OpIntercept. Make sure you set `OpIntercept.CacheResult`")
  }
  
}
