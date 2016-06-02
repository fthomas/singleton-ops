package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

class PlusSpec extends Properties("Plus") {
  property("1 + 2 == 3") = secure {
    val p1 = Plus[W.`1`.T, W.`2`.T]
    sameType[p1.Out, W.`3`.T]
  }

  property("0.2 + 0.2 == 0.4") = secure {
    val p1 = Plus[W.`0.2`.T, W.`0.2`.T]
    sameType[p1.Out, W.`0.4`.T]
  }

  property("(1 + 2) + 3 == 6") = secure {
    val p1 = Plus[W.`1`.T, W.`2`.T]
    val p2 = Plus[p1.Out, W.`3`.T]
    sameType[p2.Out, W.`6`.T]
  }

  property("1.0 + (2.0 + 3.0) == 6.0") = secure {
    val p1 = Plus[W.`2.0`.T, W.`3.0`.T]
    val p2 = Plus[W.`1.0`.T, p1.Out]
    sameType[p2.Out, W.`6.0`.T]
  }

  property("(1 + 2) + (3 + 4) == 10") = secure {
    val p1 = Plus[W.`1`.T, W.`2`.T]
    val p2 = Plus[W.`3`.T, W.`4`.T]
    val p3 = Plus[p1.Out, p2.Out]
    sameType[p3.Out, W.`10`.T]
  }

  property("W(1) + W(2) == 3") = secure {
    val w1 = W(1)
    val w2 = W(2)
    val p1 = Plus[w1.T, w2.T]
    sameType[p1.Out, W.`3`.T]
  }
}
