package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class LessThanSpec extends Properties("LessThan") {
  property("3.5F < 3.6F") = wellTyped {
    def require[P1 <: XDouble, P2 <: XDouble](implicit op : Require[P1 < P2]) : op.Out{} = op.value
    val r = require[3.5, 3.6]
  }

  property("!(5 < 4)") = wellTyped {
    def require[P1 <: XDouble, P2 <: XDouble](implicit op : Require[P1 < P2]) : op.Out{} = op.value
    illTyped(""" val r = require[3.6, 3.5] """)
  }
}
