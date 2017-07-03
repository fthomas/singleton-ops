//package singleton.ops
//
//import org.scalacheck.Properties
//import singleton.TestUtils._
//
//class MinSpec extends Properties("Min") {
//  property("Nat, left is minimum") = verifyOp2Args[Min,shapeless.Nat._1, shapeless.Nat._2, 1]
//  property("Nat, right is minimum") = verifyOp2Args[Min,shapeless.Nat._3, shapeless.Nat._2, 2]
//  property("Nat, equal") = verifyOp2Args[Min,shapeless.Nat._3, shapeless.Nat._3, 3]
//  property("Int, left is minimum") = verifyOp2Args[Min,1, 2, 1]
//  property("Int, right is minimum") = verifyOp2Args[Min,3, 2, 2]
//  property("Int, equal") = verifyOp2Args[Min,3, 3, 3]
//  property("Long, left is minimum") = verifyOp2Args[Min,1L, 2L, 1L]
//  property("Long, right is minimum") = verifyOp2Args[Min,3L, 2L, 2L]
//  property("Long, equal") = verifyOp2Args[Min,3L, 3L, 3L]
//  property("Float, left is minimum") = verifyOp2Args[Min,1.0f, 2.0f, 1.0f]
//  property("Float, right is minimum") = verifyOp2Args[Min,3.0f, 2.0f, 2.0f]
//  property("Float, equal") = verifyOp2Args[Min,3.0f, 3.0f, 3.0f]
//  property("Double, left is minimum") = verifyOp2Args[Min,1.0, 2.0, 1.0]
//  property("Double, right is minimum") = verifyOp2Args[Min,3.0, 2.0, 2.0]
//  property("Double, equal") = verifyOp2Args[Min,3.0, 3.0, 3.0]
//}