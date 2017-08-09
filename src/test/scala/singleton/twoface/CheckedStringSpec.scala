package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedStringSpec {
  type Cond[T, P] = Length[T] < P
  type Msg[T, P] = W.`"Length of string '"`.T + T + W.`"' is not smaller than "`.T + ToString[P]
  @checked1Param[Cond, Msg, String, Int] class CheckedLengthSmallerThan[T, P]
  illTyped("""@checked1Param[Cond, Msg, String, Int] trait CheckedLengthSmallerThanBad[T, P]""")
}

class CheckedStringSpec extends Properties("Checked.String") {
  import CheckedStringSpec._

  def foo[T](t : TwoFace.String[T]) : Unit = {}
  def lengthSmallerThan5[T](t : CheckedLengthSmallerThan[T,W.`5`.T]) : Unit = {
    val temp : String = t
    foo(t.unsafeCheck(5))
  }

  property("Compile-time checks") = wellTyped {
    lengthSmallerThan5("Hi")
    lengthSmallerThan5(TwoFace.String("Hi"))
    CheckedLengthSmallerThan[W.`"Hi"`.T, W.`5`.T]
    implicitly[CheckedLengthSmallerThan[W.`"Hi"`.T, W.`5`.T]]
    illTyped("""lengthSmallerThan5("Hello")""","Length of string 'Hello' is not smaller than 5")
    illTyped("""lengthSmallerThan5(TwoFace.String("Hello"))""","Length of string 'Hello' is not smaller than 5")
    illTyped("""CheckedLengthSmallerThan[W.`"Hello"`.T, W.`5`.T]""","Length of string 'Hello' is not smaller than 5")
    illTyped("""implicitly[CheckedLengthSmallerThan[W.`"Hello"`.T, W.`5`.T]]""","Length of string 'Hello' is not smaller than 5")
  }

  property("Run-time checks") = wellTyped {
    lengthSmallerThan5(us("Hi"))
    lengthSmallerThan5(TwoFace.String(us("Hi")))
    illRun{lengthSmallerThan5(us("Hello"))}
    illRun{lengthSmallerThan5(TwoFace.String(us("Hello")))}
  }

  def lengthSmallerThan5Impl[T](realValue : String)(implicit t : CheckedLengthSmallerThan.Shell[T,W.`5`.T]) : Unit =
    {t(realValue).unsafeCheck(5)}

  property("Shell compile-time checks") = wellTyped {
    lengthSmallerThan5Impl[W.`"Hi"`.T](us("Hi"))
    illTyped("""lengthSmallerThan5Impl[W.`"Hello"`.T](us("Hello"))""", "Length of string 'Hello' is not smaller than 5")
  }

  property("Shell run-time checks") = wellTyped {
    lengthSmallerThan5Impl[String](us("Hi"))
    illRun{lengthSmallerThan5Impl[String](us("Hello"))}
  }

}
