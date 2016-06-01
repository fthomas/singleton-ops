package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

class ReverseSpec extends Properties("Reverse") {
  property("abc.reverse == cba") = secure {
    val r1 = Reverse[W.`"abc"`.T]
    sameType[r1.Out, W.`"cba"`.T]
  }

  property("abc.reverse.reverse == abc") = secure {
    val r1 = Reverse[W.`"abc"`.T]
    val r2 = Reverse[r1.Out]
    sameType[r2.Out, W.`"abc"`.T]
  }
}
