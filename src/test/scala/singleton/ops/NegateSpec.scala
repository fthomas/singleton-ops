package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class NegateSpec extends Properties("Negate") {
  property("~2 == -2") = secure {
    val n1 = Negate[2]
    sameType[n1.Out, -2]
  }

  property("~1.5 == -1.5") = secure {
    val n1 = Negate[1.5]
    sameType[n1.Out, -1.5]
  }

  property("~(~5L) == 5L") = wellTyped {
    illTyped("""
      val n1 = Negate[Int, 5L]
      val n2 = Negate[Int, n1.Out]
      sameType[n2.Out, 5L]
    """)
  }

  property("Negate[1].value <: Int") = wellTyped {
    val n1 = Negate[1]
    def foo(i: Int) = ()
    foo(n1.value)
  }
}
