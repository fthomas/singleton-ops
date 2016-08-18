package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class SubstringSpec extends Properties("Substring") {
  property("foobar.substring(3) == bar") = {
    val s1 = Substring["foobar", 3]
    sameType[s1.Out, W.`"bar"`.T]
  }
}
