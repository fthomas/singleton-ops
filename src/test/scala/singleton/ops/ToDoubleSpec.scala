package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class ToDoubleSpec extends Properties("ToDouble") {
  property("3.toDouble") = wellTyped {
    def toDouble[P1 <: XInt](implicit op : ToDouble[P1]) : op.Out{} = op.value
    val r : 3.0 = toDouble[3]
  }
}