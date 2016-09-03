package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

class ReverseSpec extends Properties("Reverse") {
  property("abc.reverse == cba") = secure {
    val r1 = Reverse["abc"]
    sameType[r1.Out, "cba"]
  }

  property("abc.reverse.reverse == abc") = secure {
    val r1 = Reverse["abc"]
    val r2 = Reverse[r1.Out]
    sameType[r2.Out, "abc"]
  }
}
