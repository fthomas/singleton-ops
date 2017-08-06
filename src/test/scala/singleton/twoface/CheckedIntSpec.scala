package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

object CheckedIntSpec {

  type Cond[T] = T < W.`50`.T
  type Msg[T] = W.`"Failed Check"`.T
  @checked0Param[Cond, Msg, Int] class CheckedSmallerThan50[T]
}

class CheckedIntSpec extends Properties("Checked.Int") {
  import CheckedIntSpec._

  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {t.unsafeCheck()}

  property("Compile-time checks") = wellTyped {
    CheckedSmallerThan50(5)
    CheckedSmallerThan50[W.`5`.T]
    CheckedSmallerThan50(TwoFace.Int(5))
    val a = CheckedSmallerThan50[W.`5`.T + W.`3`.T]
    implicitly[a.T <:< W.`8`.T]
    implicitly[CheckedSmallerThan50[W.`5`.T]]
    val b = implicitly[CheckedSmallerThan50[W.`5`.T + W.`3`.T]]
    implicitly[b.T <:< (W.`5`.T + W.`3`.T)]
    smallerThan50(40)
    smallerThan50(TwoFace.Int(40))
    smallerThan50(TwoFace.Int[W.`30`.T])
    smallerThan50(implicitly[TwoFace.Int[W.`30`.T]])

    illTyped("""CheckedSmallerThan50(50)""" ,"Failed Check")
    illTyped("""CheckedSmallerThan50[W.`50`.T]""" ,"Failed Check")
    illTyped("""CheckedSmallerThan50[W.`49`.T + W.`3`.T]""" ,"Failed Check")
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
