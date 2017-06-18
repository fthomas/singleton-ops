//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//class CheckedFloatSpec extends Properties("Checked.Float") {
//  type CondSmallerThan50[T, P] = T < P
//  type MsgSmallerThan50[T, P] = "Failed Check"
//  type Param50 = 50.0f
//  type CheckedSmallerThan50[T] = Checked.Float[T, CondSmallerThan50, Param50, MsgSmallerThan50]
//
//  implicit object RuntimeChecked extends Checked.Runtime[Float, Float, CondSmallerThan50, MsgSmallerThan50] {
//    def cond(l : Float, p : Option[Float]) : scala.Boolean = l < 50.0f
//    def msg(l : Float, p : Option[Float]) : java.lang.String = s"Failed Check"
//  }
//
//  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}
//
//  property("Compile-time checks") = wellTyped {
//    smallerThan50(40.0f)
//    smallerThan50(TwoFace.Float(40.0f))
//    illTyped("""smallerThan50(50.0f)""")
//    illTyped("""smallerThan50(TwoFace.Float(50.0f))""")
//  }
//
//  property("Run-time checks") = wellTyped {
//    smallerThan50(us(40.0f))
//    smallerThan50(TwoFace.Float(us(40.0f)))
//    illRun{smallerThan50(us(50.0f))}
//    illRun{smallerThan50(TwoFace.Float(us(50.0f)))}
//  }
//}
