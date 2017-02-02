package singleton.ops

import org.scalacheck.Properties
import singleton.ops.TestUtils._

class PlusSpec extends Properties("Plus") {
  //  implicitly[Sum100[99, 2]] //Error (cannot find implicit)
  property("1 + 2 == 3") = wellTyped {
    val ev = implicitly[Require[1 + 2 == 3]]
  }

  property("0.2 + 0.2 == 0.4") = wellTyped {
    val ev = implicitly[Require[0.2 + 0.2 == 0.4]]
  }

  property("(1 + 2) + 3 == 6") = wellTyped {
    val ev = implicitly[Require[(1 + 2) + 3 == 6]]
  }

  property("1.0 + (2.0 + 3.0) == 6.0") = wellTyped {
    val ev = implicitly[Require[1.0 + (2.0 + 3.0) == 6.0]]
  }

  property("(1 + 2) + (3 + 4) == 10") = wellTyped {
    val ev = implicitly[Require[(1 + 2) + (3 + 4) == 10]]
  }

  property("Sum100_Good") = wellTyped {
    type Sum100[A, B] = Require[A + B == 100]
    val ev = implicitly[Sum100[99, 1]]
  }

}
