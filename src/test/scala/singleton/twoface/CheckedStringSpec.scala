package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedStringSpec {
  type Cond[T, P] = Length[T] < P
  type Msg[T, P] = "Length of string '" + T + "' is not smaller than " + ToString[P]
  @checked1Param[Cond, Msg, String, Int] class CheckedLengthSmallerThan[T, P]
  illTyped("""@checked1Param[Cond, Msg, String, Int] trait CheckedLengthSmallerThanBad[T, P]""")

  implicit object RuntimeChecked extends CheckedLengthSmallerThan.Runtime {
    def cond(t : String, p : Int) : scala.Boolean = t.length < p
    def msg(t : String, p : Int) : java.lang.String = "Length of string '" + t + "' is not smaller than " + p.toString
  }
}

class CheckedStringSpec extends Properties("Checked.String") {
  import CheckedStringSpec._

  def lengthSmallerThan5[T](t : CheckedLengthSmallerThan[T,5]) : Unit = {t.unsafeCheck(5)}

  property("Compile-time checks") = wellTyped {
    lengthSmallerThan5("Hi")
    lengthSmallerThan5(TwoFace.String("Hi"))
    illTyped("""smallerThan50("Hello")""")
    illTyped("""smallerThan50(TwoFace.String("Hello"))""")
  }

  property("Run-time checks") = wellTyped {
    lengthSmallerThan5(us("Hi"))
    lengthSmallerThan5(TwoFace.String(us("Hi")))
    illRun{lengthSmallerThan5(us("Hello"))}
    illRun{lengthSmallerThan5(TwoFace.String(us("Hello")))}
  }
}
