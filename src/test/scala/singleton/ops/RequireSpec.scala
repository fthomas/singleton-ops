package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class RequireSpec extends Properties("Require") {
  property("True requirement") = wellTyped {
    implicitly[Require[true]]
  }
  property("False requirement") = wellTyped {
    illTyped("""implicitly[Require[false]]""")
  }
  property("False requirement with message") = wellTyped {
    illTyped("""implicitly[RequireMsg[false,"Testing 123"]]""","Testing 123")
  }
  property("False requirement with message redirected to different symbol") = wellTyped {
    @scala.annotation.implicitNotFound("Not replaced")
    trait TestRequireMsg
    object TestRequireMsg {
      implicit def ev(implicit r : RequireMsg[false,"Testing 123"]) :
      TestRequireMsg = new TestRequireMsg {}
    }
    illTyped("""implicitly[TestRequireMsg]""","Not replaced")

    @scala.annotation.implicitNotFound("Will be replaced")
    trait TestRequireMsgSym
    object TestRequireMsgSym {
      implicit def ev(implicit r : RequireMsgSym[false,"Testing 123",TestRequireMsgSym]) :
      TestRequireMsgSym = new TestRequireMsgSym {}
    }
    illTyped("""implicitly[TestRequireMsgSym]""","Testing 123")
  }
}
