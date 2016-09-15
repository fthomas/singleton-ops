package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class SubstringSpec extends Properties("Substring") {
  property("foobar.substring(3) == bar") = wellTyped {
    def substring[P1 <: String with Singleton, P2 <: Int with Singleton](implicit op : Substring[P1, P2]) : op.Out{} = op.value
    val r : "bar" = substring["foobar", 3]
  }
}
