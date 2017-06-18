//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//class CheckedDoubleSpec extends Properties("Checked.Double") {
//  type CondSmallerThan50[T, P] = T < P
//  type MsgSmallerThan50[T, P] = "Failed Check"
//  type Param50 = 50.0
//  type CheckedSmallerThan50[T] = Checked.Double[T, CondSmallerThan50, Param50, MsgSmallerThan50]
//
//  implicit object RuntimeChecked extends Checked.Runtime[Double, Double, CondSmallerThan50, MsgSmallerThan50] {
//    def cond(l : Double, p : Option[Double]) : scala.Boolean = l < 50.0
//    def msg(l : Double, p : Option[Double]) : java.lang.String = s"Failed Check"
//  }
//
//  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}
//
//  property("Compile-time checks") = wellTyped {
//    smallerThan50(40.0)
//    smallerThan50(TwoFace.Double(40.0))
//    illTyped("""smallerThan50(50.0)""")
//    illTyped("""smallerThan50(TwoFace.Double(50.0))""")
//  }
//
//  property("Run-time checks") = wellTyped {
//    smallerThan50(us(40.0))
//    smallerThan50(TwoFace.Double(us(40.0)))
//    illRun{smallerThan50(us(50.0))}
//    illRun{smallerThan50(TwoFace.Double(us(50.0)))}
//  }
//}
