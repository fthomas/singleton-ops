package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class TimesSpec extends Properties("Times") {
  property("2 * 3 == 6") = wellTyped {
    def times[P1 <: Int with Singleton, P2 <: Int with Singleton](implicit op : P1 * P2) : op.Out{} = op.value
    val r : 6 = times[2, 3]
  }

  property("1.5 * 2.0 == 3.0") = wellTyped {
    def times[P1 <: Double with Singleton, P2 <: Double with Singleton](implicit op : P1 * P2) : op.Out{} = op.value
    val r : 3.0 = times[1.5, 2.0]
  }
}
