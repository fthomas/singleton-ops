package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class RequireSpec extends Properties("Require") {
  val Testing123 = W("Testing 123")
  type Testing123 = Testing123.T
  
  property("True requirement") = wellTyped {
    implicitly[Require[True]]
  }
  property("False requirement") = wellTyped {
    illTyped("""implicitly[Require[False]]""")
  }
  property("False requirement with message") = wellTyped {
    illTyped("""implicitly[RequireMsg[False,Testing123]]""","Testing 123")
  }
  property("False requirement with message redirected to different symbol") = wellTyped {
    @scala.annotation.implicitNotFound("Will be replaced")
    trait TestRequireMsg
    object TestRequireMsg {
      implicit def ev(implicit r : RequireMsg[False, Testing123]) :
      TestRequireMsg = new TestRequireMsg {}
    }

    trait Foo {
      trait FF[T]
    }

    object TestFoo extends Foo {

      implicit def ev[T](implicit r: RequireMsg[False, Testing123]): Set[FF[T]] = ???
    }

    illTyped("""implicitly[TestRequireMsg]""","Testing 123")
    illTyped("implicitly[Set[TestFoo.FF[Int]]]", "Testing 123")
    illTyped("""implicitly[Set[Int]]""","could not find implicit value.*")

    @scala.annotation.implicitNotFound("Not replaced")
    trait TestRequireMsgSymNotReplaced
    object TestRequireMsgSymNotReplaced {
      implicit def ev(implicit r : RequireMsgSym[False,Testing123,_]) :
      TestRequireMsgSymNotReplaced = new TestRequireMsgSymNotReplaced {}
    }
    illTyped("""implicitly[TestRequireMsgSymNotReplaced]""","Not replaced")

    @scala.annotation.implicitNotFound("Will be replaced")
    trait TestRequireMsgSym
    object TestRequireMsgSym {
      implicit def ev(implicit r : RequireMsgSym[False,Testing123,TestRequireMsgSym]) :
      TestRequireMsgSym = new TestRequireMsgSym {}
    }
    illTyped("""implicitly[TestRequireMsgSym]""","Testing 123")
  }
}
