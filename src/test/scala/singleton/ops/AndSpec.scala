package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class AndSpec extends Properties("&&") {
  property("truth table") = wellTyped {
    implicitly[Require[(true && true) == true]]
    implicitly[Require[(true && false) == false]]
    implicitly[Require[(false && true) == false]]
    implicitly[Require[(false && false) == false]]
  }
}