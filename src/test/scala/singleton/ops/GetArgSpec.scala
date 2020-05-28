package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class GetArgSpec extends Properties("GetArgSpec") {
  trait Conv {
    val value : Any
  }

  implicit class ConvInt(val value : Int)(implicit chk : CompileTime[GetArg0 < W.`50`.T]) extends Conv
  def smallerThan50(f : Conv) : Unit = {}

  implicit class BadConvLong(val value : Long)(implicit f : GetArg[W.`10`.T]) extends Conv

  implicit class FromInt[V <: Int](value : V) {
    def extendable[L <: Int](implicit left : GetLHSArg.Aux[W.`0`.T, L]) : FromInt[L] = new FromInt[L](left.value)
  }

  property("smallerThan50(1) OK") = wellTyped {
    smallerThan50(1)
  }
  property("smallerThan50(51) Compile-time fail") = wellTyped {
    illTyped("smallerThan50(51)")
  }
  property("Unsupported GetArg") = wellTyped {
    illTyped("implicitly[GetArg0]") //cannot be invoked without implicit conversion
    illTyped("smallerThan50(1L)") //Argument index too large in `BadConvLong`
    illTyped("implicitly[GetArg[W.`0.1`.T]]") //Bad argument (Double instead of Int)
  }
  property("Implicit conversion with applied type") = wellTyped {
    1.extendable
  }

  object ByName
  def byName(arg : => Any)(implicit getArg : GetArg0) = getArg
  property("By name argument not evaluated until value is read") = {
    var byNameNotRead = true
    val b = byName({byNameNotRead = false; ByName})
    val preRead = byNameNotRead
    val forceRead = b.value //now lazily invoked
    preRead && !byNameNotRead
  }

  val one = 1
  val two = 2
  val three = 2

  def testA[T, Out](value0 : T, value1 : T)(value2 : T)(value3 : T)(implicit ga : GetArg.Aux[W.`2`.T, Out]) = ga
  final val a = testA(10, 11)(12)(13)

  def testB[Out](value0 : Any, value1 : Any)(value2 : Any)(value3 : Any)(implicit ga : GetArg.Aux[W.`3`.T, Out]) = ga
  final val b = testB(one+one, two+two)(BigInt(3) + three)(13)

  property("GetArg.Aux OK") = wellTyped {
    implicitly[a.Out =:= W.`12`.T]
    implicitly[b.Out =:= W.`13`.T]
  }

  def bad_use[Out](implicit i : GetArg.Aux[W.`1.0`.T, Out] ) : Unit = {}
  property("Unsupported GetArg.Aux") = wellTyped {
    illTyped("bad_use")
  }

  import impl.InterpolatorTest._

  trait Foo[W] {
    def +[R](that: Foo.Able[R]) : Unit = {}
  }
  object Foo {
    class Able[R](val right: R)
    object Able {
      implicit def ofXInt[R <: Int](right : Int)(implicit arg: GetArg.Aux[W.`0`.T, R]) : Able[R] = new Able[R](arg)
      implicit def ofBar[R <: Bar](right: Bar)(implicit arg: GetArg0.Aux[R]): Able[R] = new Able[R](right.asInstanceOf[R])
    }
  }
  def foo(i: Int) = i

  property("GetArg combination compilation OK") = wellTyped {
    val z = new Foo[W.`5`.T] {}
    z + 1
    z + one
    z + (one + one)
    z + foo(1)
    z + foo(one)
    z + foo(one + one)
    Foo.Able.ofXInt(1)
    Foo.Able.ofXInt(one)
    Foo.Able.ofXInt(one + one)
    Foo.Able.ofXInt(foo(1))
    Foo.Able.ofXInt(foo(one))
    Foo.Able.ofXInt(foo(one + one))
  }

  property("GetArg with string interpolation inputs") = wellTyped {
    val z = new Foo[W.`5`.T] {}
    val b5 = bar"5"
    z + b5
    z + bar"5"
  }

}