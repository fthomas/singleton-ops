package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class GetLHSArgSpec extends Properties("GetLHSArgSpec") {
  abstract class Conv[T](val value : T) {
    type Out = T
  }

  implicit class ConvInt(val int : Int) {
    def ext(implicit g : GetLHSArg0) : g.Out = g.value
    def <= [T, Out](that : Conv[T])(implicit g : OpAuxGen[AcceptNonLiteral[GetLHSArg0], Out]) : Conv[Out] = new Conv[Out](g.value){}
    def := [T, Out](that : Conv[T])(implicit g : GetLHSArg.Aux[W.`0`.T, Out]) : Conv[Out] = new Conv[Out](g){}
  }

  property("implicit class argument with GetLHSArg0") = {
    val c = new Conv(2){}
    val out = 12 <= c
    implicitly[out.Out =:= W.`12`.T]
    val out2 : W.`12`.T = 12.ext
    out.value == 12
  }

  property("implicit class argument with GetLHSArg.Aux") = {
    val c = new Conv(2){}
    val out = 12 := c
    implicitly[out.Out =:= W.`12`.T]
    out.value == 12
  }

  def bad(i : Int)(implicit arg : GetLHSArg0) : Unit ={}

  property("no LHS arguments fails macro") = wellTyped {
    illTyped("""bad(4)""", "Left-hand-side tree not found")
  }

  property("Unsupported GetLHSArg") = wellTyped {
    illTyped("implicitly[GetLHSArg[W.`-1`.T]]") //Bad argument (Negative Int)
    illTyped("implicitly[GetLHSArg[W.`0.1`.T]]") //Bad argument (Double instead of Int)
  }

  class Foo(value0 : Any, value1 : Any)(value2 : Any)(value3 : Any){
    def getArg[I <: Int](idx : I)(implicit g : GetLHSArg[I]) : g.Out = g.value
  }

  property("Multiple LHS arguments") = wellTyped {
    val out0 : W.`1`.T = new Foo(1, "me")(3L)(true) getArg W(0).value
    val out1 : W.`"me"`.T  = new Foo(1, "me")(3L)(true) getArg W(1).value
    val out2 : W.`3L`.T  = new Foo(1, "me")(3L)(true) getArg W(2).value
    val out3 : W.`true`.T  = new Foo(1, "me")(3L)(true) getArg W(3).value
  }


}