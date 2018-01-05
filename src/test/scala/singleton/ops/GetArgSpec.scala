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

  property("smallerThan50(1) OK") = wellTyped {
    smallerThan50(1)
  }
  property("smallerThan50(51) Compile-time fail") = wellTyped {
    illTyped("smallerThan50(51)")
  }
  property("Unsupported GetArg") = wellTyped {
    illTyped("implicitly[GAT0]") //cannot be invoked without implicit conversion
    illTyped("smallerThan50(1L)") //Argument index too large in `BadConvLong`
    illTyped("implicitly[GetArg[W.`0.1`.T]]") //Bad argument (Double instead of Int)
  }


  def test[T, Out](value0 : T, value1 : T)(value2 : T)(value3 : T)(implicit ga : GetArg.Aux[W.`2`.T, Out]) : Out = ga.value

  final val a = test(10, 11)(12)(13)

  property("GetArg.Aux OK") = wellTyped {
    implicitly[a.type =:= W.`12`.T]
  }

  def bad_use[Out](implicit i : GetArg.Aux[W.`1.0`.T, Out] ) : Unit = {}
  property("Unsupported GetArg.Aux") = wellTyped {
    illTyped("bad_use")
  }

}