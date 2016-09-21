package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class NegateSpec extends Properties("Negate") {
  property("~2 == -2") = wellTyped {
    def negate[P1 <: XInt](implicit op : Negate[P1]) : op.Out{} = op.value
    val r : -2 = negate[2]
  }

  property("~1.5 == -1.5") = wellTyped {
    def negate[P1 <: XDouble](implicit op : Negate[P1]) : op.Out{} = op.value
    val r : -1.5 = negate[1.5]
  }

  property("~(~5L) == 5L") = wellTyped {
    def negate[P1 <: XLong](implicit op : Negate[Negate[P1]]) : op.Out{} = op.value
    val r : 5L = negate[5L]
  }
}
