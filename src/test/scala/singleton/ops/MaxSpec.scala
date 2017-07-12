package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._
import singleton.ops.math._

class MaxSpec extends Properties("Max") {
  property("Nat, right is maximum") = verifyOp2Args[Max,shapeless.Nat._1, shapeless.Nat._2, 2]
  property("Nat, left is maximum") = verifyOp2Args[Max,shapeless.Nat._3, shapeless.Nat._2, 3]
  property("Nat, equal") = verifyOp2Args[Max,shapeless.Nat._3, shapeless.Nat._3, 3]
  property("Int, right is maximum") = verifyOp2Args[Max,1, 2, 2]
  property("Int, left is maximum") = verifyOp2Args[Max,3, 2, 3]
  property("Int, equal") = verifyOp2Args[Max,3, 3, 3]
  property("Long, right is maximum") = verifyOp2Args[Max,1L, 2L, 2L]
  property("Long, left is maximum") = verifyOp2Args[Max,3L, 2L, 3L]
  property("Long, equal") = verifyOp2Args[Max,3L, 3L, 3L]
  property("Float, right is maximum") = verifyOp2Args[Max,1.0f, 2.0f, 2.0f]
  property("Float, left is maximum") = verifyOp2Args[Max,3.0f, 2.0f, 3.0f]
  property("Float, equal") = verifyOp2Args[Max,3.0f, 3.0f, 3.0f]
  property("Double, right is maximum") = verifyOp2Args[Max,1.0, 2.0, 2.0]
  property("Double, left is maximum") = verifyOp2Args[Max,3.0, 2.0, 3.0]
  property("Double, equal") = verifyOp2Args[Max,3.0, 3.0, 3.0]
}