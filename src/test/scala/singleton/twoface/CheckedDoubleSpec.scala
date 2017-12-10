package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedDoubleSpec {
  object SmallerThan50 {
    type Cond[T] = T < W.`50.0`.T
    type Msg[T] = W.`"Failed Check"`.T
    final class Checked[T](val value : Double) extends AnyVal with Checked0Param.Double.CC[Checked, Cond, Msg, T] {
      @inline def getValue : Double = value
    }
    object Checked extends Checked0Param.Double.CO[Checked, Cond, Msg]
    object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
  }
}

class CheckedDoubleSpec extends Properties("Checked.Double") {
  import CheckedDoubleSpec._

  def smallerThan50[T](t : SmallerThan50.Checked[T]) : Unit = {t.unsafeCheck()}

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
