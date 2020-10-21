package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class BitwiseAndSpec extends Properties("BitwiseAnd") {
  property("Int checks") = wellTyped {
    implicitly[Require[(W.`9`.T BitwiseAnd W.`1`.T) == W.`1`.T]]
    implicitly[Require[(W.`9`.T BitwiseAnd W.`8`.T) == W.`8`.T]]
    implicitly[Require[(W.`9`.T BitwiseAnd W.`9`.T) == W.`9`.T]]
    implicitly[Require[(W.`9`.T BitwiseAnd W.`0`.T) == W.`0`.T]]
  }
  property("Long checks") = wellTyped {
    implicitly[Require[(W.`9L`.T BitwiseAnd W.`1L`.T) == W.`1L`.T]]
    implicitly[Require[(W.`9L`.T BitwiseAnd W.`8L`.T) == W.`8L`.T]]
    implicitly[Require[(W.`9L`.T BitwiseAnd W.`9L`.T) == W.`9L`.T]]
    implicitly[Require[(W.`9L`.T BitwiseAnd W.`0L`.T) == W.`0L`.T]]
  }
}