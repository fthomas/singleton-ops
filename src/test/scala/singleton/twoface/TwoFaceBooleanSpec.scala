package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class TwoFaceBooleanSpec extends Properties("TwoFace.Boolean") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.Boolean[W.`true`.T]]
    a.getValue && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.Boolean[W.`true`.T]
    a.getValue && a.isLiteral
  }
  property("Safe Creation()") = {
    val a = TwoFace.Boolean(true)
    a.getValue && a.isLiteral
  }
  property("Unsafe Creation()") = {
    val a = TwoFace.Boolean(us(true))
    a.getValue && !a.isLiteral
  }
  property("Safe ifThenElse") = verifyTFBoolean(ifThenElse(true, false, true), false)
  property("Unsafe ifThenElse") = verifyTFBoolean(ifThenElse(us(false), false, true), us(true))

  property("Safe Boolean") = verifyTFBoolean(!TwoFace.Boolean(true), false)
  property("!Unsafe Boolean") = verifyTFBoolean(!TwoFace.Boolean(us(false)), us(true))

  property("Safe Boolean == Regular Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(true) == (true), true)
  property("Safe Boolean == Regular Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(false) == (us(false)), us(true))
  property("Unsafe Boolean == Regular Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(true)) == (true), us(true))
  property("Unsafe Boolean == Regular Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) == (us(false)), us(true))

  property("Safe Boolean != Regular Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(false) != (false), false)
  property("Safe Boolean != Regular Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(false) != (us(false)), us(false))
  property("Unsafe Boolean != Regular Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) != (false), us(false))
  property("Unsafe Boolean != Regular Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) != (us(false)), us(false))

  property("Safe Boolean == Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(true) == TwoFace.Boolean(true), true)
  property("Safe Boolean == Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(false) == TwoFace.Boolean(us(false)), us(true))
  property("Unsafe Boolean == Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(true)) == TwoFace.Boolean(true), us(true))
  property("Unsafe Boolean == Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) == TwoFace.Boolean(us(false)), us(true))

  property("Safe Boolean != Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(true) != TwoFace.Boolean(true), false)
  property("Safe Boolean != Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(false) != TwoFace.Boolean(us(false)), us(false))
  property("Unsafe Boolean != Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(true)) != TwoFace.Boolean(true), us(false))
  property("Unsafe Boolean != Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) != TwoFace.Boolean(us(false)), us(false))

  property("Safe Boolean && Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(true) && TwoFace.Boolean(true), true)
  property("Safe Boolean (true) && Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(true) && TwoFace.Boolean(us(true)), us(true))
  property("Safe Boolean (false) && Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(false) && TwoFace.Boolean(us(true)), false)
  property("Unsafe Boolean && Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) && TwoFace.Boolean(true), us(false))
  property("Unsafe Boolean && Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) && TwoFace.Boolean(us(false)), us(false))

  property("Safe Boolean || Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(true) || TwoFace.Boolean(true), true)
  property("Safe Boolean (true) || Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(true) || TwoFace.Boolean(us(true)), true)
  property("Safe Boolean (false) || Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(false) || TwoFace.Boolean(us(true)), us(true))
  property("Unsafe Boolean || Safe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) || TwoFace.Boolean(true), us(true))
  property("Unsafe Boolean || Unsafe Boolean") = verifyTFBoolean(TwoFace.Boolean(us(false)) || TwoFace.Boolean(us(false)), us(false))

  property("Safe toStringTF") = verifyTFString(TwoFace.Boolean(true).toStringTF, "true")
  property("Unsafe toStringTF") = verifyTFString(TwoFace.Boolean(us(false)).toStringTF, us("false"))
  property("Safe toSymbol") = {
    val sym = TwoFace.Boolean(false).toSymbol
    sym == scala.Symbol("false")
  }

  property("Safe require") = wellTyped {
    require(TwoFace.Boolean(true), "something")
    illTyped("""require(TwoFace.Boolean(false), "something")""","something")
    illRun(require(TwoFace.Boolean(us(false)), us("something")))
  }
  property("Unsafe require") = wellTyped {
    require(TwoFace.Boolean(us(true)), "something")
    illRun(require(TwoFace.Boolean(us(false)), "something"))
  }

  property("Implicit Conversions") = wellTyped {
    val a : TwoFace.Boolean[W.`true`.T] = implicitly[TwoFace.Boolean[W.`true`.T || W.`false`.T]]
    val b : TwoFace.Boolean[W.`true`.T || W.`false`.T] = implicitly[TwoFace.Boolean[W.`true`.T && W.`true`.T]]
    val c : TwoFace.Boolean[W.`true`.T && W.`true`.T] = implicitly[TwoFace.Boolean[W.`true`.T]]
    val d : W.`true`.T = TwoFace.Boolean(true)
    val e : Boolean = TwoFace.Boolean(us(false))
    val f : TwoFace.Boolean[Boolean] = false
  }

  property("Wrong Implicit Conversions") = wellTyped {
    illTyped("""val impl = implicitly[TwoFace.Boolean[W.`false`.T && W.`true`.T]]; val a : TwoFace.Boolean[W.`true`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.Boolean[W.`true`.T]]; val b : TwoFace.Boolean[W.`false`.T && W.`true`.T] = impl""")
  }

  property("ToString") = {
    TwoFace.Boolean[W.`true`.T].toString() == "true"
  }

  type Fin = W.`true`.T
  final val fin = true

  property("Extracting from an Upper Bounded Numeric") = wellTyped {
    def foo[W](width: TwoFace.Boolean[W]) = width
    def foo2[R <: Boolean](r: R) = foo(r)
    val a = foo2(W(fin).value)
    implicitly[a.Out =:= Fin]
    val b = foo2(us(fin))
    implicitly[b.Out =:= Boolean]
  }

  property("Extracting from Safe TwoFace") = {
    val a = me(TwoFace.Boolean(fin))
    val ret = shapeless.the[Id[a.T]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  def noImplFoo[W](w : TwoFace.Boolean[W]) = !w //Missing twoface shell implicit
  property("Unavailable Implicit Safe TwoFace Shell") = {
    val ret = noImplFoo(true)
    implicitly[ret.Out <:< ![W.`true`.T]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< W.`false`.T]
    retSimple.getValue == false
  }
  property("Unavailable Implicit Unsafe TwoFace Shell") = {
    val ret = noImplFoo(us(true))
    implicitly[ret.Out <:< ![Boolean]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< Boolean]
    retSimple.getValue == false
  }
}