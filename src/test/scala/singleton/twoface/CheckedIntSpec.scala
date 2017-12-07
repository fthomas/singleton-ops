package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedIntSpec {
  object SmallerThan50 {
    type Cond[T] = T < W.`50`.T
    type Msg[T] = W.`"Failed Check"`.T
    final class Check[T](val value : Int) extends AnyVal with Checked0Param.Int.CC[Check, Cond, Msg, T] {
      @inline def getValue : Int = value
    }
    object Check extends Checked0Param.Int.CO[Check, Cond, Msg]
    object WorkAround extends impl.Checked0ParamAny.Builder[Nothing, Nothing, Nothing, Nothing]
  }
}

class CheckedIntSpec extends Properties("Checked.Int") {
  import CheckedIntSpec._

  def foo[T](t : TwoFace.Int[T]) = t
  def smallerThan50[T](t : SmallerThan50.Check[T]) = {
    val a : Int = t
    foo(t.unsafeCheck())
  }


  property("Compile-time checks") = wellTyped {
    SmallerThan50.Check(5)
    SmallerThan50.Check[W.`5`.T]
    SmallerThan50.Check(TwoFace.Int(5))
    val a = SmallerThan50.Check[W.`5`.T + W.`3`.T]
    implicitly[a.Out <:< W.`8`.T]
    implicitly[SmallerThan50.Check[W.`5`.T]]
    val b = implicitly[SmallerThan50.Check[W.`5`.T + W.`3`.T]]
    implicitly[b.Out <:< (W.`5`.T + W.`3`.T)]
    val c = smallerThan50(40)
    implicitly[c.Out <:< W.`40`.T]
    smallerThan50(TwoFace.Int(40))
    smallerThan50(TwoFace.Int[W.`30`.T])
    smallerThan50(implicitly[TwoFace.Int[W.`30`.T]])

    illTyped("""SmallerThan50.Check(50)""" ,"Failed Check")
    illTyped("""SmallerThan50.Check[W.`50`.T]""" ,"Failed Check")
    illTyped("""SmallerThan50.Check[W.`49`.T + W.`3`.T]""" ,"Failed Check")
    illTyped("""smallerThan50(50)""" ,"Failed Check")
    illTyped("""smallerThan50(TwoFace.Int(50))""", "Failed Check")
  }

  property("Run-time checks") = wellTyped {
    smallerThan50(us(40))
    smallerThan50(TwoFace.Int(us(40)))
    illRun{smallerThan50(us(50))}
    illRun{smallerThan50(TwoFace.Int(us(50)))}
  }
}
