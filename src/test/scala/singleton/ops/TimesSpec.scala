package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class TimesSpec extends Properties("Times") {
  property("2 * 3 == 6") = secure {
    val t1 = Times[W.`2`.T, W.`3`.T]
    sameType[t1.Out, W.`6`.T]
  }

  property("1.5 * 2.0 == 3.0") = secure {
    val t1 = Times[W.`1.5`.T, W.`2.0`.T]
    sameType[t1.Out, W.`3.0`.T]
  }

  property("Int * 0 = ???") = wellTyped {
    illTyped(""" Times[Int, W.`0`.T] """)
  }

  property("(1 with Int) * 0 = ???") = wellTyped {
    illTyped(""" Times[W.`1`.T with Int, W.`0`.T] """)
  }
}
