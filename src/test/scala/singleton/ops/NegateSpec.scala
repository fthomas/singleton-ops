package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class NegateSpec extends Properties("Negate") {
  property("~2 == -2") = wellTyped {
    def negate[P1 <: Int with Singleton](implicit op : Negate[P1]) : op.Out{} = op.value
    val r : -2 = negate[2]
  }

  property("~1.5 == -1.5") = wellTyped {
    def negate[P1 <: Double with Singleton](implicit op : Negate[P1]) : op.Out{} = op.value
    val r : -1.5 = negate[1.5]
  }

  property("~(~5L) == 5L") = wellTyped {
    def negate[P1 <: Long with Singleton](implicit op : Negate[Negate[P1]]) : op.Out{} = op.value
    val r : 5L = negate[5L]
  }
}
