package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedBooleanSpec {
  type Cond[T] = T
  type Msg[T] = "Failed Check"
  @checked0Param[Cond, Msg, Boolean] class CheckedTrue[T]
  illTyped("""@checked0Param[Cond, Msg, Boolean] trait CheckedTrueBad[T]""")

  implicit object RuntimeChecked extends CheckedTrue.Runtime {
    def cond(l : Boolean) : scala.Boolean = l
    def msg(l : Boolean) : java.lang.String = s"Failed Check"
  }
}

class CheckedBooleanSpec extends Properties("Checked.Boolean") {
  import CheckedBooleanSpec._

  def condTrue[T](t : CheckedTrue[T]) : Unit = {t.unsafeCheck()}

  property("Compile-time checks") = wellTyped {
    condTrue(true)
    condTrue(TwoFace.Boolean(true))
    illTyped("""smallerThan50(false)""")
    illTyped("""smallerThan50(TwoFace.Boolean(false))""")
  }

  property("Run-time checks") = wellTyped {
    condTrue(us(true))
    condTrue(TwoFace.Boolean(us(true)))
    illRun{condTrue(us(false))}
    illRun{condTrue(TwoFace.Boolean(us(false)))}
  }
}
