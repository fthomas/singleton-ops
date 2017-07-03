//package singleton.ops
//
//import org.scalacheck.Properties
//import singleton.TestUtils._
//
//class ITESpec extends Properties("ITE") {
//  property("True condition") = wellTyped {
//    implicitly[Require[ITE[true, 1, 2] == 1]]
//  }
//  property("False condition") = wellTyped {
//    implicitly[Require[ITE[false, 1, 2] == 2]]
//  }
//}