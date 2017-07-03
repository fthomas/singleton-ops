//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//object CheckedIntSpec {
//  type Cond[T] = T < 50
//  type Msg[T] = "Failed Check"
//  @checked0Param[Cond, Msg, Int] class CheckedSmallerThan50[T]
//
//  implicit object RuntimeChecked extends CheckedSmallerThan50.Runtime {
//    def cond(l : Int) : scala.Boolean = l < 50
//    def msg(l : Int) : java.lang.String = s"Failed Check"
//  }
//}
//
//class CheckedIntSpec extends Properties("Checked.Int") {
//  import CheckedIntSpec._
//
//  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}
//
//  property("Compile-time checks") = wellTyped {
//    smallerThan50(40)
//    smallerThan50(TwoFace.Int(40))
//    illTyped("""smallerThan50(50)""")
//    illTyped("""smallerThan50(TwoFace.Int(50))""")
//  }
//
//  property("Run-time checks") = wellTyped {
//    smallerThan50(us(40))
//    smallerThan50(TwoFace.Int(us(40)))
//    illRun{smallerThan50(us(50))}
//    illRun{smallerThan50(TwoFace.Int(us(50)))}
//  }
//}
