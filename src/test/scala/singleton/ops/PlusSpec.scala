package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

class PlusSpec extends Properties("Plus") {
  property("1 + 2 == 3") = secure {
    val plus = Plus[W.`1`.T, W.`2`.T]
    sameType[plus.Out, W.`3`.T]
  }

  property("0.2 + 0.2 == 0.4") = secure {
    val plus = Plus[W.`0.2`.T, W.`0.2`.T]
    sameType[plus.Out, W.`0.4`.T]
  }
}
