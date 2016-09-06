//package singleton.ops
//
//import org.scalacheck.Prop._
//import org.scalacheck.Properties
//import singleton.ops.TestUtils._
//
//class ToDoubleSpec extends Properties("ToDouble") {
//  property("3.toDouble") = secure {
//    val t1 = ToDouble[3]
//    sameType[t1.Out, 3.0] && t1.value == 3.0
//  }
//
//  property("ToDouble.value <: Double") = wellTyped {
//    def foo(d: Double) = d * d
//    def bar[A](t: ToDouble[A]) = foo(t.value)
//  }
//}
