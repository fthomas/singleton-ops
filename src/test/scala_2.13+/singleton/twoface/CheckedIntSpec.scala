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
  def smallerThan50Seq(tSeq : SmallerThan50.Checked[Int]*) = {
    for (t <- tSeq) {
      val a : Int = t
      foo(t.unsafeCheck())
    }
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
    smallerThan50Seq(1,2,3)
    val widen : SmallerThan50.Checked[Int] = 5
    illTyped("""smallerThan50(50)""" ,"Failed Check")
    illTyped("""smallerThan50(TwoFace.Int(50))""", "Failed Check")
    illTyped("""implicitly[SmallerThan50.Checked[W.`60`.T]]""", "Failed Check")
    illTyped("""val widen2 : SmallerThan50.Checked[Int] = 50""" ,"Failed Check")
    illTyped("""smallerThan50Seq(1,us(70),60)""","Failed Check")
  }

  property("Run-time checks") = wellTyped {
    smallerThan50Seq(1,2,3)
    smallerThan50Seq(us(1),2,3)
    smallerThan50(us(40))
    smallerThan50(TwoFace.Int(us(40)))
    val us70 = 70
    val us60 = 60
    illRun{smallerThan50(us(50))}
    illRun{smallerThan50(TwoFace.Int(us(50)))}
    illRun{smallerThan50Seq(1,us70,us60)}
  }

  //Maybe at fault of Scalac, but some schemes prove better than others to avoid errors
  property("Stability check") = wellTyped {
    class Fooish[T]
    class Foo {
      final def foo : Fooish[W.`4`.T] = new Fooish[W.`4`.T]
      final def foo[T](value : SmallerThan50.Checked[T]) : Fooish[value.Out] = new Fooish[value.Out]
    }
    val a = new Foo
    val b : Fooish[W.`2`.T] = a.foo(2)
    val two = 2
    val c : Fooish[Int] = a.foo(two)
  }
}
