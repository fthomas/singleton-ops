package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class ReverseSpec extends Properties("Reverse") {
  property("abc.reverse == cba") = wellTyped {
    def reverse[P1 <: XString](implicit op : Reverse[P1]) : op.Out{} = op.value
    val r : W.`"cba"`.T = reverse[W.`"abc"`.T]
  }

  property("abc.reverse.reverse == abc") = wellTyped {
    def reverse[P1 <: XString](implicit op : Reverse[Reverse[P1]]) : op.Out{} = op.value
    val r : W.`"abc"`.T = reverse[W.`"abc"`.T]
  }
}
