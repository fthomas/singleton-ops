package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class OrSpec extends Properties("||") {
  property("truth table") = wellTyped {
    implicitly[Require[(True || True) == True]]
    implicitly[Require[(True || False) == True]]
    implicitly[Require[(False || True) == True]]
    implicitly[Require[(False || False) == False]]
  }
}