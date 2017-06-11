package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class EqSpec extends Properties("==") {
  property("Boolean arguments") = wellTyped {
    implicitly[Require[true == true]]
    implicitly[Require[false == false]]
  }
  property("False requirement") = {
    illTyped("""implicitly[Require[false]]""");
    true
  }
}
