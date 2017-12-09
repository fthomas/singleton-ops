package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedFloatSpec {
  object SmallerThan50 {
    type Cond[T] = T < W.`50.0f`.T
    type Msg[T] = W.`"Failed Check"`.T
    final class Checked[T](val value : Float) extends AnyVal with Checked0Param.Float.CC[Checked, Cond, Msg, T] {
      @inline def getValue : Float = value
    }
    object Checked extends Checked0Param.Float.CO[Checked, Cond, Msg]
    object WorkAround extends singleton.twoface.impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
  }
}

class CheckedFloatSpec extends Properties("Checked.Float") {
  import CheckedFloatSpec._

  def smallerThan50[T](t : SmallerThan50.Checked[T]) : Unit = {t.unsafeCheck()}

  property("Compile-time checks") = wellTyped {
    smallerThan50(40.0f)
    smallerThan50(TwoFace.Float(40.0f))
    illTyped("""smallerThan50(50.0f)""")
    illTyped("""smallerThan50(TwoFace.Float(50.0f))""")
  }

  property("Run-time checks") = wellTyped {
    smallerThan50(us(40.0f))
    smallerThan50(TwoFace.Float(us(40.0f)))
    illRun{smallerThan50(us(50.0f))}
    illRun{smallerThan50(TwoFace.Float(us(50.0f)))}
  }
}
