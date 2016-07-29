package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class SubstringSpec extends Properties("Substring") {
  property("foobar.substring(3) == bar") = {
    val s1 = Substring[W.`"foobar"`.T, W.`3`.T]
    sameType[s1.Out, W.`"bar"`.T]
  }
}
