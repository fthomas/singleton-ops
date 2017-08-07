package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._
import singleton.ops.math._

class MaxSpec extends Properties("Max") {
  property("Nat, right is maximum") = verifyOp2Args[Max,shapeless.Nat._1, shapeless.Nat._2, W.`2`.T]
  property("Nat, left is maximum") = verifyOp2Args[Max,shapeless.Nat._3, shapeless.Nat._2, W.`3`.T]
  property("Nat, equal") = verifyOp2Args[Max,shapeless.Nat._3, shapeless.Nat._3, W.`3`.T]
  property("Int, right is maximum") = verifyOp2Args[Max, W.`1`.T, W.`2`.T, W.`2`.T]
  property("Int, left is maximum") = verifyOp2Args[Max, W.`3`.T, W.`2`.T, W.`3`.T]
  property("Int, equal") = verifyOp2Args[Max, W.`3`.T, W.`3`.T, W.`3`.T]
  property("Long, right is maximum") = verifyOp2Args[Max, W.`1L`.T, W.`2L`.T, W.`2L`.T]
  property("Long, left is maximum") = verifyOp2Args[Max, W.`3L`.T, W.`2L`.T, W.`3L`.T]
  property("Long, equal") = verifyOp2Args[Max, W.`3L`.T, W.`3L`.T, W.`3L`.T]
  property("Float, right is maximum") = verifyOp2Args[Max, W.`1.0f`.T, W.`2.0f`.T, W.`2.0f`.T]
  property("Float, left is maximum") = verifyOp2Args[Max, W.`3.0f`.T, W.`2.0f`.T, W.`3.0f`.T]
  property("Float, equal") = verifyOp2Args[Max, W.`3.0f`.T, W.`3.0f`.T, W.`3.0f`.T]
  property("Double, right is maximum") = verifyOp2Args[Max, W.`1.0`.T, W.`2.0`.T, W.`2.0`.T]
  property("Double, left is maximum") = verifyOp2Args[Max, W.`3.0`.T, W.`2.0`.T, W.`3.0`.T]
  property("Double, equal") = verifyOp2Args[Max, W.`3.0`.T, W.`3.0`.T, W.`3.0`.T]
}