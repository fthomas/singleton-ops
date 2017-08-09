package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedBooleanSpec {
  type Cond[T] = T
  type Msg[T] = W.`"Failed Check"`.T
  @checked0Param[Cond, Msg, Boolean] class CheckedTrue[T]
  illTyped("""@checked0Param[Cond, Msg, Boolean] trait CheckedTrueBad[T]""")
}

class CheckedBooleanSpec extends Properties("Checked.Boolean") {
  import CheckedBooleanSpec._

  def condTrue[T](t : CheckedTrue[T]) : Unit = {t.unsafeCheck()}

  property("Compile-time checks") = wellTyped {
    condTrue(true)
    condTrue(TwoFace.Boolean(true))
    illTyped("""condTrue(false)""")
    illTyped("""condTrue(TwoFace.Boolean(false))""")
  }

  property("Run-time checks") = wellTyped {
    condTrue(us(true))
    condTrue(TwoFace.Boolean(us(true)))
    illRun{condTrue(us(false))}
    illRun{condTrue(TwoFace.Boolean(us(false)))}
  }

  def condTrueImpl[T](realValue : Boolean)(implicit t : CheckedTrue.Shell[T]) : Unit = {t(realValue).unsafeCheck()}

  property("Shell compile-time checks") = wellTyped {
    condTrueImpl[True](true)
    illTyped("""condTrueImpl[False](true)""", "Failed Check")
    illTyped("""condTrueImpl[False](false)""", "Failed Check")
  }

  property("Shell run-time checks") = wellTyped {
    condTrueImpl[Boolean](true)
    illRun{condTrueImpl[Boolean](false)}
  }
}
