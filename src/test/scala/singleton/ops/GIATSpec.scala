package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class GIATSpec extends Properties("GIATSpec") {
  trait Conv {
    val value : Any
  }
  type CheckSmallThan50 = CompileTime[GIAT0 < W.`50`.T]

  implicit class ConvInt(val value : Int)(implicit chk : CheckSmallThan50) extends Conv
  def smallerThan50(f : Conv) : Unit = {}

  implicit class BadConvLong(val value : Long)(implicit f : GetImplicitArgType[W.`10`.T]) extends Conv


  property("smallerThan50(1) OK") = wellTyped {
    smallerThan50(1)
  }
  property("smallerThan50(51) Compile-time fail") = wellTyped {
  }
  property("Unsupported") = wellTyped {
    illTyped("implicitly[GIAT0]") //cannot be invoked without implicit conversion
    illTyped("smallerThan50(1L)") //Argument index too large in `BadConvLong`
    illTyped("implicitly[GetImplicitArgType[W.`0.1`.T]]") //Bad argument (Double instead of Int)
  }
}