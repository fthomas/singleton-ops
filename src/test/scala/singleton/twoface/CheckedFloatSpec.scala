//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//object CheckedFloatSpec {
//  type Cond[T] = T < 50.0f
//  type Msg[T] = "Failed Check"
//  @checked0Param[Cond, Msg, Float] class CheckedSmallerThan50[T]
//
//  implicit object RuntimeChecked extends CheckedSmallerThan50.Runtime {
//    def cond(l : Float) : scala.Boolean = l < 50.0f
//    def msg(l : Float) : java.lang.String = s"Failed Check"
//  }
//}
//
//class CheckedFloatSpec extends Properties("Checked.Float") {
//  import CheckedFloatSpec._
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
