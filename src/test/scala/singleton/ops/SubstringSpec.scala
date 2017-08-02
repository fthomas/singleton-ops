package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class SubstringSpec extends Properties("Substring") {
  property("foobar.substring(3) == bar") = wellTyped {
    def substring[P1 <: XString, P2 <: XInt](implicit op : Substring[P1, P2]) : op.Out{} = op.value
    val r : W.`"bar"`.T = substring[W.`"foobar"`.T, W.`3`.T]
  }
}
