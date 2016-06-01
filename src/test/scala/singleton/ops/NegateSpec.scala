package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

class NegateSpec extends Properties("Negate") {
  property("~2 == -2") = secure {
    val n1 = Negate[W.`2`.T]
    sameType[n1.Out, W.`-2`.T]
  }

  property("~1.5 == -1.5") = secure {
    val n1 = Negate[W.`1.5`.T]
    sameType[n1.Out, W.`-1.5`.T]
  }

  /*
  property("~(~5L) == 5L") = secure {
    val n1 = Negate[W.`5L`.T]
    val n2 = Negate[n1.Out]
    sameType[n2.Out, W.`5L`.T]
  }
 */
}
