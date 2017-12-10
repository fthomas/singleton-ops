package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedIntSpec {
  object SmallerThan50 extends Checked0Param.Int {
    type Cond[T] = T < W.`50`.T
    type Msg[T] = W.`"Failed Check"`.T
  }
}

class CheckedIntSpec extends Properties("Checked.Int") {
  import CheckedIntSpec._

  def foo[T](t : TwoFace.Int[T]) = t
  def smallerThan50[T](t : SmallerThan50.Checked[T]) = {
    val a : Int = t
    foo(t.unsafeCheck())
  }


  property("Compile-time checks") = wellTyped {
    implicitly[SmallerThan50.Checked[W.`5`.T]]
    val b = implicitly[SmallerThan50.Checked[W.`5`.T + W.`3`.T]]
    implicitly[b.Out <:< (W.`5`.T + W.`3`.T)]
    val c = smallerThan50(40)
    implicitly[c.Out <:< W.`40`.T]
    smallerThan50(TwoFace.Int(40))
    smallerThan50(TwoFace.Int[W.`30`.T])
    smallerThan50(implicitly[TwoFace.Int[W.`30`.T]])
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
