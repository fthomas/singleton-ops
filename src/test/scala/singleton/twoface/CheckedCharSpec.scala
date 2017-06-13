package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class CheckedCharSpec extends Properties("Checked.Char") {
  type CondSmallerThan50[T, P] = T < P
  type MsgSmallerThan50[T, P] = "Failed Check"
  type Param50 = '\u0032'
  type CheckedSmallerThan50[T] = Checked.Char[T, CondSmallerThan50, Param50, MsgSmallerThan50]

  implicit object RuntimeChecked extends Checked.Runtime[Char, Char, CondSmallerThan50, MsgSmallerThan50] {
    def cond(l : Char, p : Option[Char]) : scala.Boolean = l < '\u0032'
    def msg(l : Char, p : Option[Char]) : java.lang.String = s"Failed Check"
  }

  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}

  property("Compile-time checks") = wellTyped {
    smallerThan50('\u0020')
    smallerThan50(TwoFace.Char('\u0020'))
    illTyped("""smallerThan50('\u0032')""")
    illTyped("""smallerThan50(TwoFace.Char('\u0032'))""")
  }

  property("Run-time checks") = wellTyped {
    smallerThan50(us('\u0020'))
    smallerThan50(TwoFace.Char(us('\u0020')))
    illRun{smallerThan50(us('\u0032'))}
    illRun{smallerThan50(TwoFace.Char(us('\u0032')))}
  }
}
