package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class LessThanSpec extends Properties("LessThan") {
  property("3.5F < 3.6F") = secure {
    LessThan[W.`3.5F`.T, W.`3.6F`.T].value
  }

  property("!(5 < 4)") = wellTyped {
    illTyped(""" LessThan[W.`5`.T, W.`4`.T] """)
  }
}
