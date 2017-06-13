package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class CheckedBooleanSpec extends Properties("Checked.Boolean") {
  type CondTrue[T, P] = T == P
  type MsgTrue[T, P] = "Failed Check"
  type Param = true
  type CheckedTrue[T] = Checked.Boolean[T, CondTrue, Param, MsgTrue]

  implicit object RuntimeChecked extends Checked.Runtime[Boolean, Boolean, CondTrue, MsgTrue] {
    def cond(l : Boolean, p : Option[Boolean]) : scala.Boolean = l
    def msg(l : Boolean, p : Option[Boolean]) : java.lang.String = s"Failed Check"
  }

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
