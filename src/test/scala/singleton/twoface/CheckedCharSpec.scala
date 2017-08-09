//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//object CheckedCharSpec {
//  type Cond[T] = T < W.`'\u0032'`.T
//  type Msg[T] = W.`"Failed Check"`.T
//  @checked0Param[Cond, Msg, Char] class CheckedSmallerThan50[T]
//}
//
//class CheckedCharSpec extends Properties("Checked.Char") {
//  import CheckedCharSpec._
//
//  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}
//
//  property("Compile-time checks") = wellTyped {
//    smallerThan50('\u0020')
//    smallerThan50(TwoFace.Char('\u0020'))
//    illTyped("""smallerThan50('\u0032')""")
//    illTyped("""smallerThan50(TwoFace.Char('\u0032'))""")
//  }
//
//  property("Run-time checks") = wellTyped {
//    smallerThan50(us('\u0020'))
//    smallerThan50(TwoFace.Char(us('\u0020')))
//    illRun{smallerThan50(us('\u0032'))}
//    illRun{smallerThan50(TwoFace.Char(us('\u0032')))}
//  }
//}
