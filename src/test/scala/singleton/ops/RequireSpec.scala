package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class RequireSpec extends Properties("Require") {
  property("True requirement") = wellTyped {
    implicitly[Require[true]]
  }

  property("False requirement") = {
    illTyped("""implicitly[Require[false]]""")
    true
  }
}
