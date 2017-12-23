package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class TwoFaceStringSpec extends Properties("TwoFace.String") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.String[W.`"Something"`.T]]
    a.getValue == "Something" && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.String[W.`"Something"`.T]
    a.getValue == "Something" && a.isLiteral
  }
  property("Safe Creation()") = {
    val a = TwoFace.String("Something")
    a.getValue == "Something" && a.isLiteral
  }
  property("Unsafe Creation()") = {
    val a = TwoFace.String(us("Something"))
    a.getValue == "Something" && !a.isLiteral
  }

  property("Safe ifThenElse") = verifyTFString(ifThenElse(true, "Hi", "Hello"), "Hi")
  property("Unsafe ifThenElse") = verifyTFString(ifThenElse(us(false), "Hi", "Hello"), us("Hello"))

  property("Safe String + Safe String") = verifyTFString(TwoFace.String("Some") + TwoFace.String("thing"), "Something")
  property("Safe String + Unsafe String") = verifyTFString(TwoFace.String("Some") + TwoFace.String(us("thing")), us("Something"))
  property("Unsafe String + Safe String") = verifyTFString(TwoFace.String(us("Some")) + TwoFace.String("thing"), us("Something"))
  property("Unsafe String + Unsafe String") = verifyTFString(TwoFace.String(us("Some")) + TwoFace.String(us("thing")), us("Something"))

  property("Safe String == Regular Safe String") = verifyTFBoolean(TwoFace.String("Some") == ("Some"), true)
  property("Safe String == Regular Unsafe String") = verifyTFBoolean(TwoFace.String("Some") == (us("Some")), us(true))
  property("Unsafe String == Regular Safe String") = verifyTFBoolean(TwoFace.String(us("Some")) == ("Some"), us(true))
  property("Unsafe String == Regular Unsafe String") = verifyTFBoolean(TwoFace.String(us("Some")) == (us("Some")), us(true))

  property("Safe String != Regular Safe String") = verifyTFBoolean(TwoFace.String("Some") != ("Some"), false)
  property("Safe String != Regular Unsafe String") = verifyTFBoolean(TwoFace.String("Some") != (us("Some")), us(false))
  property("Unsafe String != Regular Safe String") = verifyTFBoolean(TwoFace.String(us("Some")) != ("Some"), us(false))
  property("Unsafe String != Regular Unsafe String") = verifyTFBoolean(TwoFace.String(us("Some")) != (us("Some")), us(false))

  property("Safe String == Safe String") = verifyTFBoolean(TwoFace.String("Some") == TwoFace.String("Some"), true)
  property("Safe String == Unsafe String") = verifyTFBoolean(TwoFace.String("Some") == TwoFace.String(us("Some")), us(true))
  property("Unsafe String == Safe String") = verifyTFBoolean(TwoFace.String(us("Some")) == TwoFace.String("Some"), us(true))
  property("Unsafe String == Unsafe String") = verifyTFBoolean(TwoFace.String(us("Some")) == TwoFace.String(us("Some")), us(true))

  property("Safe String != Safe String") = verifyTFBoolean(TwoFace.String("Some") != TwoFace.String("Some"), false)
  property("Safe String != Unsafe String") = verifyTFBoolean(TwoFace.String("Some") != TwoFace.String(us("Some")), us(false))
  property("Unsafe String != Safe String") = verifyTFBoolean(TwoFace.String(us("Some")) != TwoFace.String("Some"), us(false))
  property("Unsafe String != Unsafe String") = verifyTFBoolean(TwoFace.String(us("Some")) != TwoFace.String(us("Some")), us(false))

  property("Safe String substring Safe Int") = verifyTFString(TwoFace.String("Something") substring TwoFace.Int(4), "thing")
  property("Safe String substring Unsafe Int") = verifyTFString(TwoFace.String("Something") substring TwoFace.Int(us(4)), us("thing"))
  property("Unsafe String substring Safe Int") = verifyTFString(TwoFace.String(us("Something")) substring TwoFace.Int(4), us("thing"))
  property("Unsafe String substring Unsafe Int") = verifyTFString(TwoFace.String(us("Something")) substring TwoFace.Int(us(4)), us("thing"))

  property("Safe String charAt Safe Int") = verifyTFChar(TwoFace.String("Something") charAt TwoFace.Int(4), 't')
  property("Safe String charAt Unsafe Int") = verifyTFChar(TwoFace.String("Something") charAt TwoFace.Int(us(4)), us('t'))
  property("Unsafe String charAt Safe Int") = verifyTFChar(TwoFace.String(us("Something")) charAt TwoFace.Int(4), us('t'))
  property("Unsafe String charAt Unsafe Int") = verifyTFChar(TwoFace.String(us("Something")) charAt TwoFace.Int(us(4)), us('t'))

  property("Safe toNat") = wellTyped {
    val nat = TwoFace.String("1").toNat
    verifyOp[nat.N, shapeless.Nat._1]
  }
  property("Safe toInt") = verifyTFInt(TwoFace.String("1").toInt, 1)
  property("Unsafe toInt") = verifyTFInt(TwoFace.String(us("1")).toInt, us(1))
  property("Safe toLong") = verifyTFLong(TwoFace.String("1").toLong, 1L)
  property("Unsafe toLong") = verifyTFLong(TwoFace.String(us("1")).toLong, us(1L))
  property("Safe toFloat") = verifyTFFloat(TwoFace.String("1.0").toFloat, 1.0f)
  property("Unsafe toFloat") = verifyTFFloat(TwoFace.String(us("1.0")).toFloat, us(1.0f))
  property("Safe toDouble") = verifyTFDouble(TwoFace.String("1.0").toDouble, 1.0)
  property("Unsafe toDouble") = verifyTFDouble(TwoFace.String(us("1.0")).toDouble, us(1.0))
  property("Safe toSymbol") = {
    val sym = TwoFace.String("foo").toSymbol
    sym == 'foo
  }

  property("Safe length") = verifyTFInt(TwoFace.String("Some").length, 4)
  property("Unsafe length") = verifyTFInt(TwoFace.String(us("Some")).length, us(4))

  property("Safe reverse") = verifyTFString(TwoFace.String("Some").reverse, "emoS")
  property("Unsafe reverse") = verifyTFString(TwoFace.String(us("Some")).reverse, us("emoS"))

  property("Implicit Conversions") = wellTyped {
    val a : TwoFace.String[W.`"Something"`.T] = implicitly[TwoFace.String[W.`"Some"`.T + W.`"thing"`.T]]
    val b : TwoFace.String[W.`"Som"`.T + W.`"ething"`.T] = implicitly[TwoFace.String[W.`"Some"`.T + W.`"thing"`.T]]
    val c : TwoFace.String[W.`"Some"`.T + W.`"thing"`.T] = implicitly[TwoFace.String[W.`"Something"`.T]]
    val d : W.`"Some"`.T = TwoFace.String("Some")
    val e : String = TwoFace.String(us("Some"))
    val f : TwoFace.String[String] = "Some"
  }

  property("Wrong Implicit Conversions") = wellTyped {
    illTyped("""val impl = implicitly[TwoFace.String[W.`"Som"`.T + W.`"E"`.T]]; val a : TwoFace.String[W.`"Some"`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.String[W.`"SomeThing"`.T]]; val b : TwoFace.String[W.`"Some"`.T + W.`"thing"`.T] = impl""")
  }

  type Fin = W.`"a"`.T
  final val fin = "a"

  property("Extracting from an Upper Bounded Numeric") = wellTyped {
    def foo[W](width: TwoFace.String[W]) = width
    def foo2[R <: String](r: R) = foo(r)
    val a = foo2(W(fin).value)
    implicitly[a.Out =:= Fin]
    val b = foo2(us(fin))
    implicitly[b.Out =:= String]
  }

  property("Extracting from Safe TwoFace") = {
    val a = me(TwoFace.String(fin))
    val ret = shapeless.the[Id[a.T]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  def noImplFoo[W](w : TwoFace.String[W]) = w + w //Missing twoface shell implicit
  property("Unavailable Implicit Safe TwoFace Shell") = {
    val ret = noImplFoo("Ma")
    implicitly[ret.Out <:< +[W.`"Ma"`.T,W.`"Ma"`.T]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< W.`"MaMa"`.T]
    retSimple.getValue == "MaMa"
  }
  property("Unavailable Implicit Unsafe TwoFace Shell") = {
    val ret = noImplFoo(us("Ma"))
    implicitly[ret.Out <:< +[String, String]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< String]
    retSimple.getValue == "MaMa"
  }
}