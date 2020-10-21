package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class BitwiseOrSpec extends Properties("BitwiseOr") {
  property("Int checks") = wellTyped {
    implicitly[Require[(W.`9`.T BitwiseOr W.`1`.T) == W.`9`.T]]
    implicitly[Require[(W.`9`.T BitwiseOr W.`8`.T) == W.`9`.T]]
    implicitly[Require[(W.`1`.T BitwiseOr W.`8`.T) == W.`9`.T]]
    implicitly[Require[(W.`9`.T BitwiseOr W.`0`.T) == W.`9`.T]]
  }
  property("Long checks") = wellTyped {
    implicitly[Require[(W.`9L`.T BitwiseOr W.`1L`.T) == W.`9L`.T]]
    implicitly[Require[(W.`9L`.T BitwiseOr W.`8L`.T) == W.`9L`.T]]
    implicitly[Require[(W.`1L`.T BitwiseOr W.`8L`.T) == W.`9L`.T]]
    implicitly[Require[(W.`9L`.T BitwiseOr W.`0L`.T) == W.`9L`.T]]
  }
}