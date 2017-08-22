package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class ConcatSpec extends Properties("Concat") {
  property("ab + cd == abcd") = wellTyped {
    def concat[P1 <: XString, P2 <: XString](implicit op : P1 + P2) : op.Out{} = op.value
    val r : W.`"abcd"`.T = concat[W.`"ab"`.T, W.`"cd"`.T]
  }
}
