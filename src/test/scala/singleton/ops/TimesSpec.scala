package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class TimesSpec extends Properties("Times") {
  property("2 * 3 == 6") = secure {
    val t1 = Times[2, 3]
    sameType[t1.Out, 6]
  }

  property("1.5 * 2.0 == 3.0") = secure {
    val t1 = Times[1.5, 2.0]
    sameType[t1.Out, 3.0]
  }

  property("Int * 0 = ???") = wellTyped {
    illTyped(""" Times[Int, 0] """)
  }

  property("(1 with Int) * 0 = ???") = wellTyped {
    illTyped(""" Times[1 with Int, 0] """)
  }
}
