package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedBooleanSpec {
  object JustTrue extends Checked0Param.Boolean {
    type Cond[T] = T
    type Msg[T] = W.`"Failed Check"`.T
  }
}

class CheckedBooleanSpec extends Properties("Checked.Boolean") {
  import CheckedBooleanSpec._

  def condTrue[T](t : JustTrue.Checked[T]) : Unit = {t.unsafeCheck()}

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

  def condTrueImpl[T](realValue : Boolean)(implicit t : JustTrue.CheckedShell[T]) : Unit = {t.unsafeCheck(realValue)}

  property("Shell compile-time checks") = wellTyped {
    condTrueImpl[True](true)
    illTyped("""condTrueImpl[False](true)""", "Failed Check")
    illTyped("""condTrueImpl[False](false)""", "Failed Check")
  }

  property("Shell run-time checks") = wellTyped {
    condTrueImpl[Boolean](true)
    illRun{condTrueImpl[Boolean](false)}
  }

  trait CheckedUse[T]
  object CheckedUse {
    implicit def ev[T](implicit checkedTrue: JustTrue.CheckedShellSym[CheckedUse[_], T]) : CheckedUse[T] =
      new CheckedUse[T] {}
  }

  property("Shell user message redirect checks") = wellTyped {
    implicitly[CheckedUse[True]]
    illTyped("""implicitly[CheckedUse[False]]""", "Failed Check")
  }
}
