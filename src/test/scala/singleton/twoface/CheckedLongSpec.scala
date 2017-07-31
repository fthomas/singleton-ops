package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedLongSpec {
  type Cond[T] = T < 50L
  type Msg[T] = "Failed Check"
  @checked0Param[Cond, Msg, Long] class CheckedSmallerThan50[T]
}

class CheckedLongSpec extends Properties("Checked.Long") {
  import CheckedLongSpec._

  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}

  property("Compile-time checks") = wellTyped {
    smallerThan50(40L)
    smallerThan50(TwoFace.Long(40L))
    illTyped("""smallerThan50(50L)""")
    illTyped("""smallerThan50(TwoFace.Long(50L))""")
  }

  property("Run-time checks") = wellTyped {
    smallerThan50(us(40L))
    smallerThan50(TwoFace.Long(us(40L)))
    illRun{smallerThan50(us(50L))}
    illRun{smallerThan50(TwoFace.Long(us(50L)))}
  }
}
