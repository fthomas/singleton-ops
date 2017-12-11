package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedLongSpec {
  object SmallerThan50 extends Checked0Param.Long {
    type Cond[T] = T < W.`50L`.T
    type Msg[T] = W.`"Failed Check"`.T
  }
}

class CheckedLongSpec extends Properties("Checked.Long") {
  import CheckedLongSpec._

  def smallerThan50[T](t : SmallerThan50.Checked[T]) : Unit = {t.unsafeCheck()}

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
