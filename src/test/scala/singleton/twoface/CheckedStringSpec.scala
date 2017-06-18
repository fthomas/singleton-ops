//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//class CheckedStringSpec extends Properties("Checked.String") {
//  type CondCheckedLengthSmallerThan5[T, P] = Length[T] < P
//  type MsgCheckedLengthSmallerThan5[T, P] = "Failed Check"
//  type Param = 5
//  type CheckedLengthSmallerThan5[T] = Checked.String[T, CondCheckedLengthSmallerThan5, Param, MsgCheckedLengthSmallerThan5]
//
//  implicit object RuntimeChecked extends Checked.Runtime[String, Int, CondCheckedLengthSmallerThan5, MsgCheckedLengthSmallerThan5] {
//    def cond(l : String, p : Option[Int]) : scala.Boolean = l.length < 5
//    def msg(l : String, p : Option[Int]) : java.lang.String = s"Failed Check"
//  }
//
//  def lengthSmallerThan5[T](t : CheckedLengthSmallerThan5[T]) : Unit = {t.unsafeCheck()}
//
//  property("Compile-time checks") = wellTyped {
//    lengthSmallerThan5("Hi")
//    lengthSmallerThan5(TwoFace.String("Hi"))
//    illTyped("""smallerThan50("Hello")""")
//    illTyped("""smallerThan50(TwoFace.String("Hello"))""")
//  }
//
//  property("Run-time checks") = wellTyped {
//    lengthSmallerThan5(us("Hi"))
//    lengthSmallerThan5(TwoFace.String(us("Hi")))
//    illRun{lengthSmallerThan5(us("Hello"))}
//    illRun{lengthSmallerThan5(TwoFace.String(us("Hello")))}
//  }
//}
