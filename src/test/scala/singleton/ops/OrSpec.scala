package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class OrSpec extends Properties("||") {
  property("truth table") = wellTyped {
    implicitly[Require[(true || true) == true]]
    implicitly[Require[(true || false) == true]]
    implicitly[Require[(false || true) == true]]
    implicitly[Require[(false || false) == false]]
  }
}