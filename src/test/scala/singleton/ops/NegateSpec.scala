package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._
import shapeless.test.illTyped

class NegateSpec extends Properties("Negate") {
  property("Nat argument") = wellTyped {
    implicitly[Require[Negate[shapeless.Nat._1] == W.`(-1)`.T]]
  }

  property("Char argument") = wellTyped {
    implicitly[Require[Negate[W.`'T'`.T] == W.`(-84)`.T]]
  }

  property("Int argument") = wellTyped {
    implicitly[Require[Negate[W.`2`.T] == W.`(-2)`.T]]
    implicitly[Require[Negate[W.`-2`.T] == W.`2`.T]]
  }

  property("Long argument") = wellTyped {
    implicitly[Require[Negate[W.`5L`.T] == W.`(-5L)`.T]]
    implicitly[Require[Negate[W.`-5L`.T] == W.`5L`.T]]
  }

  property("Float argument") = wellTyped {
    implicitly[Require[Negate[W.`1.5f`.T] == W.`(-1.5f)`.T]]
    implicitly[Require[Negate[W.`-1.5f`.T] == W.`1.5f`.T]]
  }

  property("Double argument") = wellTyped {
    implicitly[Require[Negate[W.`1.5`.T] == W.`(-1.5)`.T]]
    implicitly[Require[Negate[W.`-1.5`.T] == W.`1.5`.T]]
  }

  property("Boolean argument") = {
    illTyped("""implicitly[Negate[W.`true`.T]]""")
    true
  }

  property("String argument") = {
    illTyped("""implicitly[Negate[W.`"Something"`.T]]""")
    true
  }
}
