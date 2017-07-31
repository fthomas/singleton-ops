package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class TwoFaceBooleanSpec extends Properties("TwoFace.Boolean") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.Boolean[true]]
    a.getValue && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.Boolean[true]
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
  property("Safe ifThenElse") = verifyTF(ifThenElse(true, false, true), false)
  property("Unsafe ifThenElse") = verifyTF(ifThenElse(us(false), false, true), us(true))

  property("Safe Boolean") = verifyTF(!TwoFace.Boolean(true), false)
  property("!Unsafe Boolean") = verifyTF(!TwoFace.Boolean(us(false)), us(true))

  property("Safe Boolean == Regular Safe Boolean") = verifyTF(TwoFace.Boolean(true) == (true), true)
  property("Safe Boolean == Regular Unsafe Boolean") = verifyTF(TwoFace.Boolean(false) == (us(false)), us(true))
  property("Unsafe Boolean == Regular Safe Boolean") = verifyTF(TwoFace.Boolean(us(true)) == (true), us(true))
  property("Unsafe Boolean == Regular Unsafe Boolean") = verifyTF(TwoFace.Boolean(us(false)) == (us(false)), us(true))

  property("Safe Boolean == Safe Boolean") = verifyTF(TwoFace.Boolean(true) == TwoFace.Boolean(true), true)
  property("Safe Boolean == Unsafe Boolean") = verifyTF(TwoFace.Boolean(false) == TwoFace.Boolean(us(false)), us(true))
  property("Unsafe Boolean == Safe Boolean") = verifyTF(TwoFace.Boolean(us(true)) == TwoFace.Boolean(true), us(true))
  property("Unsafe Boolean == Unsafe Boolean") = verifyTF(TwoFace.Boolean(us(false)) == TwoFace.Boolean(us(false)), us(true))

  property("Safe Boolean != Safe Boolean") = verifyTF(TwoFace.Boolean(true) != TwoFace.Boolean(true), false)
  property("Safe Boolean != Unsafe Boolean") = verifyTF(TwoFace.Boolean(false) != TwoFace.Boolean(us(false)), us(false))
  property("Unsafe Boolean != Safe Boolean") = verifyTF(TwoFace.Boolean(us(true)) != TwoFace.Boolean(true), us(false))
  property("Unsafe Boolean != Unsafe Boolean") = verifyTF(TwoFace.Boolean(us(false)) != TwoFace.Boolean(us(false)), us(false))

  property("Safe Boolean && Safe Boolean") = verifyTF(TwoFace.Boolean(true) && TwoFace.Boolean(true), true)
  property("Safe Boolean && Unsafe Boolean") = verifyTF(TwoFace.Boolean(false) && TwoFace.Boolean(us(true)), us(false))
  property("Unsafe Boolean && Safe Boolean") = verifyTF(TwoFace.Boolean(us(false)) && TwoFace.Boolean(true), us(false))
  property("Unsafe Boolean && Unsafe Boolean") = verifyTF(TwoFace.Boolean(us(false)) && TwoFace.Boolean(us(false)), us(false))

  property("Safe Boolean || Safe Boolean") = verifyTF(TwoFace.Boolean(true) || TwoFace.Boolean(true), true)
  property("Safe Boolean || Unsafe Boolean") = verifyTF(TwoFace.Boolean(false) || TwoFace.Boolean(us(true)), us(true))
  property("Unsafe Boolean || Safe Boolean") = verifyTF(TwoFace.Boolean(us(false)) || TwoFace.Boolean(true), us(true))
  property("Unsafe Boolean || Unsafe Boolean") = verifyTF(TwoFace.Boolean(us(false)) || TwoFace.Boolean(us(false)), us(false))

  property("Safe toStringTF") = verifyTF(TwoFace.Boolean(true).toStringTF, "true")
  property("Unsafe toStringTF") = verifyTF(TwoFace.Boolean(us(false)).toStringTF, us("false"))

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
    val a : TwoFace.Boolean[true] = implicitly[TwoFace.Boolean[true || false]]
    val b : TwoFace.Boolean[true || false] = implicitly[TwoFace.Boolean[true && true]]
    val c : TwoFace.Boolean[true && true] = implicitly[TwoFace.Boolean[true]]
    val d : true = TwoFace.Boolean(true)
    val e : Boolean = TwoFace.Boolean(us(false))
  }

  property("Wrong Implicit Conversions") = {
    illTyped("""val a : TwoFace.Boolean[true] = implicitly[TwoFace.Boolean[false && true]]""")
    illTyped("""val b : TwoFace.Boolean[false && true] = implicitly[TwoFace.Boolean[true]]""")
    true
  }

  property("ToString") = {
    TwoFace.Boolean[true].toString() == "true"
  }

  type Fin = true
  final val fin = true
  property("Extracting from Safe TwoFace") = {
    val a = TwoFace.Boolean(fin)
    val ret = shapeless.the[Id[a.type]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  property("Extracting from Unsafe TwoFace") = wellTyped {
    val a = TwoFace.Boolean(us(fin))
    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
    implicitly[ret.Out =:= Boolean]
    ret.value == fin
  }
}