package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class OpInterceptSpec extends Properties("OpInterceptSpec") {

  trait Vec[A0, A1] {
    def show(implicit a0 : Id[A0], a1 : Id[A1]) : String = s"Vec[${a0.value}, ${a1.value}]"
  }

  implicit def `Vec+`[VL0, VL1, VR0, VR1](
    implicit
    opL : VL0 + VR0,
    opR : VL1 + VR1
  ) : OpIntercept.Aux[Vec[VL0, VL1] + Vec[VR0, VR1], Vec[opL.Out, opR.Out]] = //Vec is not a singleton value, so we need to instantiate OpIntercept
    new OpIntercept[Vec[VL0, VL1] + Vec[VR0, VR1]] {
      type Out = Vec[opL.Out, opR.Out]
      val value : Out = new Vec[opL.Out, opR.Out]{}
    }

  implicit def `Vec==`[VL0, VL1, VR0, VR1](
    implicit
    op : (VL0 == VR0) && (VL1 == VR1)
  ) : OpIntercept.Aux[Vec[VL0, VL1] == Vec[VR0, VR1], op.Out] = ??? //No need to instantiate when a singleton value is returned

  property("Custom Vec Equality OK") = {
    val eq1 = shapeless.the[Vec[W.`1`.T, W.`2`.T] == Vec[W.`1`.T, W.`2`.T]]
    val eq2 = shapeless.the[Vec[W.`1`.T, W.`2`.T] == Vec[W.`1`.T, W.`1`.T]]
    implicitly[eq1.Out =:= W.`true`.T]
    implicitly[eq2.Out =:= W.`false`.T]
    eq1.value == true
  }

  property("Custom Vec Addition OK") = {
    val add2 = shapeless.the[Vec[W.`1`.T, W.`2`.T] + Vec[W.`3`.T, W.`8`.T]]
    val add3 = shapeless.the[Vec[W.`1`.T, W.`2`.T] + Vec[W.`3`.T, W.`8`.T] + Vec[W.`20`.T, W.`20`.T]]
    implicitly[add2.Out =:= Vec[W.`4`.T, W.`10`.T]]
    implicitly[add3.Out =:= Vec[W.`24`.T, W.`30`.T]]
    val add23 = shapeless.the[add2.Out + add3.Out]
    implicitly[add23.Out =:= Vec[W.`28`.T, W.`40`.T]]
    add2.value.show == "Vec[4, 10]"
  }


  trait FibId
  type Fib[P] = impl.OpMacro[FibId, P, W.`0`.T, W.`0`.T]
  implicit def doFib[P](
    implicit
    op : ITE[P == W.`0`.T, W.`0`.T, ITE[P == W.`1`.T, W.`1`.T, Fib[P - W.`1`.T] + Fib[P - W.`2`.T]]]
  ) : OpIntercept.Aux[Fib[P], op.Out] = ??? //No need to instantiate when a singleton value is returned

  property("Custom Fibonacci Op OK") = {
    val fib4 = shapeless.the[Fib[W.`4`.T]]
    implicitly[fib4.Out =:= W.`3`.T]
    val fib10 = shapeless.the[Fib[W.`10`.T]]
    implicitly[fib10.Out =:= W.`55`.T]
    fib10.value == 55
  }


  trait FooOpId
  type FooOp[C, M] = impl.OpMacro[FooOpId, C, M, W.`0`.T]
  implicit def FooOp[C, M](
    implicit
    r : RequireMsg[C, M]
  ) : OpIntercept[FooOp[C, M]] = ???

  property("Error Message Propagation") = wellTyped {
    illTyped("""shapeless.the[FooOp[W.`false`.T, W.`"this is a test"`.T]]""", "this is a test")
  }
}
