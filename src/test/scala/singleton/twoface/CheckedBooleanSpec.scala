package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedBooleanSpec {
  object True {
    type Cond[T] = T
    type Msg[T] = W.`"Failed Check"`.T
    final class Check[T](val value : Boolean) extends AnyVal with Checked0Param.Boolean.CC[Check, Cond, Msg, T] {
      @inline def getValue : Boolean = value
    }
    object Check extends Checked0Param.Boolean.CO[Check, Cond, Msg]
    object WorkAround extends impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
  }
}

class CheckedBooleanSpec extends Properties("Checked.Boolean") {
  import CheckedBooleanSpec._

  def condTrue[T](t : True.Check[T]) : Unit = {t.unsafeCheck()}

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

  def condTrueImpl[T](realValue : Boolean)(implicit t : True.Check.Shell[T]) : Unit = {t.unsafeCheck(realValue)}

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
    implicit def ev[T](implicit checkedTrue: True.Check.ShellSym[CheckedUse[_], T]) : CheckedUse[T] =
      new CheckedUse[T] {}
  }

  property("Shell user message redirect checks") = wellTyped {
    implicitly[CheckedUse[True]]
    illTyped("""implicitly[CheckedUse[False]]""", "Failed Check")
  }
}
