package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class OpInterceptSpec extends Properties("OpInterceptSpec") {

  trait Vec[A0, A1] {
    def show(implicit a0 : Id[A0], a1 : Id[A1]) : String = s"Vec[${a0}, ${a1}]"
  }

  implicit def VecToVec[A0, A1] : OpInterceptAux[Vec[A0, A1] => Vec[_, _], Vec[A0, A1]] =
    new OpIntercept[Vec[A0, A1] => Vec[_,_]] {
      val value : Out = new Vec[A0, A1]{}
      type Out = Vec[A0, A1]
    }
  implicit def ScalarIntToVec[I <: XInt] : OpInterceptAux[I => Vec[_, _], Vec[I, I]] =
    new OpIntercept[I => Vec[_,_]] {
      val value : Out = new Vec[I, I]{}
      type Out = Vec[I, I]
    }

  implicit def `Vec+`[L, VL0, VL1, VL[_,_], R, VR0, VR1, VR[_,_]](
    implicit
    convL : OpInterceptAux[L => Vec[_, _], VL[VL0, VL1]],
    convR : OpInterceptAux[R => Vec[_, _], VR[VR0, VR1]],
    opL : VL0 + VR0,
    opR : VL1 + VR1,
    maxVecSize : RequireMsg[(VL0 + VR0 + VL1 + VR1) < 100, "Reached maximum vector size allowed"]
  ) : OpInterceptAux[L + R, Vec[opL.Out, opR.Out]] = //Vec is not a singleton value, so we need to instantiate OpIntercept
    new OpIntercept[L + R] {
      type Out = Vec[opL.Out, opR.Out]
      val value : Out = new Vec[opL.Out, opR.Out]{}
    }

  val vecPlusVec    = shapeless.the[Vec[1, 2] + 10]
  val vecPlusScalar = shapeless.the[1 + Vec[1, 2] + Vec[10, 10]]
  illTyped("shapeless.the[Vec[1, 2] + Vec[10, 10] + 50]", "Reached maximum vector size allowed")


  trait Time[H, M]
  trait Hours[H]
  trait Minutes[M]

  implicit def Time2Time[H, M] : OpInterceptAux[Time[H, M] => Time[_, _], Time[H, M]] = ???
  implicit def Hours2Time[H] : OpInterceptAux[Hours[H] => Time[_, _], Time[H, 0]] = ???
  implicit def Minutes2Time[M](implicit h : M / 60, m : M % 60) :
  OpInterceptAux[Minutes[M] => Time[_, _], Time[h.Out, m.Out]] = ???

  implicit def `Time+`[L, R, LH, LM, RH, RM, OH, OM, TL[_,_], TR[_,_]](
    implicit
    convL : OpInterceptAux[L => Time[_, _], TL[LH, LM]],
    convR : OpInterceptAux[R => Time[_, _], TR[RH, RM]],
    opM : OpGenAux[(LM + RM) % 60, OM],
    opH : OpGenAux[LH + RH + ((LM + RM) / 60), OH]
  ) : OpInterceptAux[L + R, Time[OH, OM]] = ???

  val totalTime = shapeless.the[Hours[2] + Time[1, 20] + Time[3, 40] + Hours[2] + Minutes[15]]
  implicitly[totalTime.Out =:= Time[9, 15]]

  sealed trait Cast
  object Cast {
    trait Char extends Cast
    trait Int extends Cast
    trait Long extends Cast
    trait Float extends Cast
    trait Double extends Cast
    trait Boolean extends Cast
    trait String extends Cast
  }
  implicit def castCI[L <: XChar,   R <: XInt]  : OpInterceptAux[(L, R) => Cast, Cast.Int] = ???
  implicit def castCL[L <: XChar,   R <: XLong] : OpInterceptAux[(L, R) => Cast, Cast.Long] = ???
  implicit def castIC[L <: XInt,    R <: XChar] : OpInterceptAux[(L, R) => Cast, Cast.Int] = ???
  implicit def castIL[L <: XInt,    R <: XLong] : OpInterceptAux[(L, R) => Cast, Cast.Long] = ???
  implicit def castLC[L <: XLong,   R <: XChar] : OpInterceptAux[(L, R) => Cast, Cast.Long] = ???
  implicit def castLI[L <: XLong,   R <: XInt]  : OpInterceptAux[(L, R) => Cast, Cast.Long] = ???

  implicit def Char2Char[C <: XChar] : OpInterceptAux[C => Cast.Char, C] = ???
  implicit def Char2Int[C <: XChar](implicit op : ToInt[C]) : OpInterceptAux[C => Cast.Int, op.Out] = ???
  implicit def Char2Long[C <: XChar](implicit op : ToLong[C]) : OpInterceptAux[C => Cast.Long, op.Out] = ???
  implicit def Int2Int[I <: XInt] : OpInterceptAux[I => Cast.Int, I] = ???
  implicit def Int2Long[I <: XInt](implicit op : ToLong[I]) : OpInterceptAux[I => Cast.Long, op.Out] = ???
  implicit def Long2Long[L <: XLong] : OpInterceptAux[L => Cast.Long, L] = ???

  implicit def `+Ext`[L, R, BO, LL, RL](
    implicit
    basicOp : OpInterceptAux[(L, R) => Cast, BO],
    convL : OpInterceptAux[L => BO, LL],
    convR : OpInterceptAux[R => BO, RL],
    op : LL + RL
  ) : OpInterceptAux[L + R, op.Out] = ???

  implicit def `-Ext`[L, R, BO, LL, RL](
    implicit
    basicOp : OpInterceptAux[(L, R) => Cast, BO],
    convL : OpInterceptAux[L => BO, LL],
    convR : OpInterceptAux[R => BO, RL],
    op : LL - RL
  ) : OpInterceptAux[L - R, op.Out] = ???

  implicit def `*Ext`[L, R, BO, LL, RL](
    implicit
    basicOp : OpInterceptAux[(L, R) => Cast, BO],
    convL : OpInterceptAux[L => BO, LL],
    convR : OpInterceptAux[R => BO, RL],
    op : LL * RL
  ) : OpInterceptAux[L * R, op.Out] = ???

  implicit def `/Ext`[L, R, BO, LL, RL](
    implicit
    basicOp : OpInterceptAux[(L, R) => Cast, BO],
    convL : OpInterceptAux[L => BO, LL],
    convR : OpInterceptAux[R => BO, RL],
    op : LL / RL
  ) : OpInterceptAux[L / R, op.Out] = ???

  implicitly[20/4 + 1L + 1 * 5L + 'c']


  implicit def `Vec==`[VL0, VL1, VR0, VR1](
    implicit
    op : (VL0 == VR0) && (VL1 == VR1)
  ) : OpInterceptAux[Vec[VL0, VL1] == Vec[VR0, VR1], op.Out] = ??? //No need to instantiate when a singleton value is returned

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
  type Fib[P] = impl.OpMacro[FibId, Tuple1[P]]
  implicit def doFib[P](
    implicit
    op : ITE[P == W.`0`.T, W.`0`.T, ITE[P == W.`1`.T, W.`1`.T, Fib[P - W.`1`.T] + Fib[P - W.`2`.T]]]
  ) : OpInterceptAux[Fib[P], op.Out] = ??? //No need to instantiate when a singleton value is returned

  property("Custom Fibonacci Op OK") = {
    val fib4 = shapeless.the[Fib[W.`4`.T]]
    implicitly[fib4.Out =:= W.`3`.T]
    val fib10 = shapeless.the[Fib[W.`10`.T]]
    implicitly[fib10.Out =:= W.`55`.T]
    fib10.value == 55
  }


  trait FooOpId
  type FooOp[C, M] = impl.OpMacro[FooOpId, (C, M)]
  implicit def FooOp[C, M](
    implicit
    r : RequireMsg[C, M]
  ) : OpIntercept[FooOp[C, M]] = ???

  property("Error Message Propagation") = wellTyped {
    illTyped("""shapeless.the[FooOp[W.`false`.T, W.`"this is a test"`.T]]""", "this is a test")
  }
}
