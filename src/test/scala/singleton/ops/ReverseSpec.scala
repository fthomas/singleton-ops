package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class ReverseSpec extends Properties("Reverse") {
  property("abc.reverse == cba") = wellTyped {
    def reverse[P1 <: String with Singleton](implicit op : Reverse[P1]) : op.Out{} = op.value
    val r : "cba" = reverse["abc"]
  }

  property("abc.reverse.reverse == abc") = wellTyped {
    def reverse[P1 <: String with Singleton](implicit op : Reverse[Reverse[P1]]) : op.Out{} = op.value
    val r : "abc" = reverse["abc"]
  }
}
