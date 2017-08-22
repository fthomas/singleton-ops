package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class AndSpec extends Properties("&&") {
  property("truth table") = wellTyped {
    implicitly[Require[(True && True) == True]]
    implicitly[Require[(True && False) == False]]
    implicitly[Require[(False && True) == False]]
    implicitly[Require[(False && False) == False]]
  }
}