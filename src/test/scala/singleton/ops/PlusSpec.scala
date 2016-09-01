package singleton.ops

import org.scalacheck.Prop._
import org.scalacheck.Properties
import singleton.ops.TestUtils._

object MyFailedTest {
  val one : 1 = 1
  def add[A <: Int with Singleton, B <: Int with Singleton](a: A, b: B)(implicit p : Plus[Int, A, B]) : p.Out {} = p.value
  val works = add(1, 1)
  val doesNotWork = add(one, one)
}

class PlusSpec extends Properties("Plus") {
  property("1 + 2 == 3") = secure {
    val p1 = Plus[Int, 1, 2]
    sameType[p1.Out, 3]
  }

  property("0.2 + 0.2 == 0.4") = secure {
    val p1 = Plus[Double, 0.2, 0.2]
    sameType[p1.Out, 0.4]
  }

  property("(1 + 2) + 3 == 6") = secure {
    val p1 = Plus[Int, 1, 2]
    val p2 = Plus[Int, p1.Out, 3]
    sameType[p2.Out, 6]
  }

  property("1.0 + (2.0 + 3.0) == 6.0") = secure {
    val p1 = Plus[Double, 2.0, 3.0]
    val p2 = Plus[Double, 1.0, p1.Out]
    sameType[p2.Out, 6.0]
  }

  property("(1 + 2) + (3 + 4) == 10") = secure {
    val p1 = Plus[Int, 1, 2]
    val p2 = Plus[Int, 3, 4]
    val p3 = Plus[Int, p1.Out, p2.Out]
    sameType[p3.Out, 10]
  }

  property("W(1) + W(2) == 3") = secure {
    val w1 = W(1)
    val w2 = W(2)
    val p1 = Plus[Int, w1.T, w2.T]
    sameType[p1.Out, 3]
  }
}
