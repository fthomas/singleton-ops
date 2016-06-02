package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

class ConcatSpec extends Properties("Concat") {
  property("ab + cd == abcd") = secure {
    val c1 = Concat[W.`"ab"`.T, W.`"cd"`.T]
    sameType[c1.Out, W.`"abcd"`.T]
  }
}
