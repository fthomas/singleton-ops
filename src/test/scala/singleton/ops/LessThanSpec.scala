//package singleton.ops
//
//import org.scalacheck.Prop._
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.ops.TestUtils._
//
//class LessThanSpec extends Properties("LessThan") {
//  property("3.5F < 3.6F") = secure {
//    LessThan[3.5F, 3.6F].value
//  }
//
//  property("!(5 < 4)") = wellTyped {
//    illTyped(""" LessThan[5, 4] """)
//  }
//}
