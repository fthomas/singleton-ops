package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class CheckedLongSpec extends Properties("Checked.Long") {
  type CondSmallerThan50[T, P] = T < P
  type MsgSmallerThan50[T, P] = "Failed Check"
  type Param50 = 50L
  type CheckedSmallerThan50[T] = Checked.Long[T, CondSmallerThan50, Param50, MsgSmallerThan50]

  implicit object RuntimeChecked extends Checked.Runtime[Long, Long, CondSmallerThan50, MsgSmallerThan50] {
    def cond(l : Long, p : Option[Long]) : scala.Boolean = l < 50L
    def msg(l : Long, p : Option[Long]) : java.lang.String = s"Failed Check"
  }

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
