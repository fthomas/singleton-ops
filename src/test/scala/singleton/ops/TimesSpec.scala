package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

class TimesSpec extends Properties("Times") {
  property("2 * 3 == 6") = secure {
    val times = Times[W.`2`.T, W.`3`.T]
    sameType[times.Out, W.`6`.T]
  }

  property("1.5 * 2.0 == 3.0") = secure {
    val times = Times[W.`1.5`.T, W.`2.0`.T]
    sameType[times.Out, W.`3.0`.T]
  }
}
