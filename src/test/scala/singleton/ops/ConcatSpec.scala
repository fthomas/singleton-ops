package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class ConcatSpec extends Properties("Concat") {
  property("ab + cd == abcd") = wellTyped {
    def concat[P1 <: String with Singleton, P2 <: String with Singleton](implicit op : P1 + P2) : op.Out{} = op.value
    val r : "abcd" = concat["ab", "cd"]
  }
}
