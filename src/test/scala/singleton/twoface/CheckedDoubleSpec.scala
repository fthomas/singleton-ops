package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedDoubleSpec {
  type Cond[T] = T < 50.0
  type Msg[T] = "Failed Check"
  @checked0Param[Cond, Msg, Double] class CheckedSmallerThan50[T]

  implicit object RuntimeChecked extends CheckedSmallerThan50.Runtime {
    def cond(l : Double) : scala.Boolean = l < 50.0
    def msg(l : Double) : java.lang.String = s"Failed Check"
  }
}

class CheckedDoubleSpec extends Properties("Checked.Double") {
  import CheckedDoubleSpec._

  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}

  property("Compile-time checks") = wellTyped {
    smallerThan50(40.0)
    smallerThan50(TwoFace.Double(40.0))
    illTyped("""smallerThan50(50.0)""")
    illTyped("""smallerThan50(TwoFace.Double(50.0))""")
  }

  property("Run-time checks") = wellTyped {
    smallerThan50(us(40.0))
    smallerThan50(TwoFace.Double(us(40.0)))
    illRun{smallerThan50(us(50.0))}
    illRun{smallerThan50(TwoFace.Double(us(50.0)))}
  }
}
