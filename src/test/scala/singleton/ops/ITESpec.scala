package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class ITESpec extends Properties("ITE") {
  property("True condition") = wellTyped {
    implicitly[Require[ITE[True, W.`1`.T, W.`2`.T] == W.`1`.T]]
  }
  property("False condition") = wellTyped {
    implicitly[Require[ITE[False, W.`1`.T, W.`2`.T] == W.`2`.T]]
  }
}