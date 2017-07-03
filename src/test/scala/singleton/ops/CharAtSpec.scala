//package singleton.ops
//
//import org.scalacheck.Properties
//import singleton.TestUtils._
//
//class CharAtSpec extends Properties("CharAt") {
//  property("foobar.charAt(3) == b") = wellTyped {
//    def charAt[P1 <: XString, P2 <: XInt](implicit op : CharAt[P1, P2]) : op.Value{} = op.value
//    val r : 'b' = charAt["foobar", 3]
//  }
//}
