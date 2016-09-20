package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class TimesSpec extends Properties("Times") {
  property("2 * 3 == 6") = wellTyped {
    def times[P1 <: XInt, P2 <: XInt](implicit op : P1 * P2) : op.Out{} = op.value
    val r : 6 = times[2, 3]
  }

  property("1.5 * 2.0 == 3.0") = wellTyped {
    def times[P1 <: XDouble, P2 <: XDouble](implicit op : P1 * P2) : op.Out{} = op.value
    val r : 3.0 = times[1.5, 2.0]
  }
}
