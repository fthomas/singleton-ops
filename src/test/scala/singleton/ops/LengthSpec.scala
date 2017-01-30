package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class LengthSpec extends Properties("Length") {
  property("foobar.length == 6") = wellTyped {
    def length[P1 <: XString](implicit op : Length[P1]) : op.Out{} = op.value
    val r : 6 = length["foobar"]
  }
}
