package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedLongSpec {
  object SmallerThan50 {
    type Cond[T] = T < W.`50L`.T
    type Msg[T] = W.`"Failed Check"`.T
    final class Checked[T](val value : Long) extends AnyVal with Checked0Param.Long.CC[Checked, Cond, Msg, T] {
      @inline def getValue : Long = value
    }
    object Checked extends Checked0Param.Long.CO[Checked, Cond, Msg]
    object WorkAround extends impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
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
