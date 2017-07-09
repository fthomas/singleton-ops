package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._
import shapeless.test.illTyped

class NegateSpec extends Properties("Negate") {
  property("Nat argument") = wellTyped {
    implicitly[Require[Negate[shapeless.Nat._1] == (-1)]]
  }

  property("Char argument") = wellTyped {
    implicitly[Require[Negate['T'] == (-84)]]
  }

  property("Int argument") = wellTyped {
    implicitly[Require[Negate[2] == (-2)]]
    implicitly[Require[Negate[-2] == 2]]
  }

  property("Long argument") = wellTyped {
    implicitly[Require[Negate[5L] == (-5L)]]
    implicitly[Require[Negate[-5L] == 5L]]
  }

  property("Float argument") = wellTyped {
    implicitly[Require[Negate[1.5f] == (-1.5f)]]
    implicitly[Require[Negate[-1.5f] == 1.5f]]
  }

  property("Double argument") = wellTyped {
    implicitly[Require[Negate[1.5] == (-1.5)]]
    implicitly[Require[Negate[-1.5] == 1.5]]
  }

  property("Boolean argument") = {
    illTyped("""implicitly[Negate[true]]""")
    true
  }

  property("String argument") = {
    illTyped("""implicitly[Negate["Something"]]""")
    true
  }
}
