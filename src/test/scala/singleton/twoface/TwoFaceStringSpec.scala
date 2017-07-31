package singleton.twoface

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class TwoFaceStringSpec extends Properties("TwoFace.String") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.String["Something"]]
    a.getValue == "Something" && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.String["Something"]
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

  property("Safe ifThenElse") = verifyTF(ifThenElse(true, "Hi", "Hello"), "Hi")
  property("Unsafe ifThenElse") = verifyTF(ifThenElse(us(false), "Hi", "Hello"), us("Hello"))

  property("Safe String + Safe String") = verifyTF(TwoFace.String("Some") + TwoFace.String("thing"), "Something")
  property("Safe String + Unsafe String") = verifyTF(TwoFace.String("Some") + TwoFace.String(us("thing")), us("Something"))
  property("Unsafe String + Safe String") = verifyTF(TwoFace.String(us("Some")) + TwoFace.String("thing"), us("Something"))
  property("Unsafe String + Unsafe String") = verifyTF(TwoFace.String(us("Some")) + TwoFace.String(us("thing")), us("Something"))

  property("Safe String == Regular Safe String") = verifyTF(TwoFace.String("Some") == ("Some"), true)
  property("Safe String == Regular Unsafe String") = verifyTF(TwoFace.String("Some") == (us("Some")), us(true))
  property("Unsafe String == Regular Safe String") = verifyTF(TwoFace.String(us("Some")) == ("Some"), us(true))
  property("Unsafe String == Regular Unsafe String") = verifyTF(TwoFace.String(us("Some")) == (us("Some")), us(true))

  property("Safe String == Safe String") = verifyTF(TwoFace.String("Some") == TwoFace.String("Some"), true)
  property("Safe String == Unsafe String") = verifyTF(TwoFace.String("Some") == TwoFace.String(us("Some")), us(true))
  property("Unsafe String == Safe String") = verifyTF(TwoFace.String(us("Some")) == TwoFace.String("Some"), us(true))
  property("Unsafe String == Unsafe String") = verifyTF(TwoFace.String(us("Some")) == TwoFace.String(us("Some")), us(true))

  property("Safe String != Safe String") = verifyTF(TwoFace.String("Some") != TwoFace.String("Some"), false)
  property("Safe String != Unsafe String") = verifyTF(TwoFace.String("Some") != TwoFace.String(us("Some")), us(false))
  property("Unsafe String != Safe String") = verifyTF(TwoFace.String(us("Some")) != TwoFace.String("Some"), us(false))
  property("Unsafe String != Unsafe String") = verifyTF(TwoFace.String(us("Some")) != TwoFace.String(us("Some")), us(false))

  property("Safe String substring Safe Int") = verifyTF(TwoFace.String("Something") substring TwoFace.Int(4), "thing")
  property("Safe String substring Unsafe Int") = verifyTF(TwoFace.String("Something") substring TwoFace.Int(us(4)), us("thing"))
  property("Unsafe String substring Safe Int") = verifyTF(TwoFace.String(us("Something")) substring TwoFace.Int(4), us("thing"))
  property("Unsafe String substring Unsafe Int") = verifyTF(TwoFace.String(us("Something")) substring TwoFace.Int(us(4)), us("thing"))

  property("Safe String charAt Safe Int") = verifyTF(TwoFace.String("Something") charAt TwoFace.Int(4), 't')
  property("Safe String charAt Unsafe Int") = verifyTF(TwoFace.String("Something") charAt TwoFace.Int(us(4)), us('t'))
  property("Unsafe String charAt Safe Int") = verifyTF(TwoFace.String(us("Something")) charAt TwoFace.Int(4), us('t'))
  property("Unsafe String charAt Unsafe Int") = verifyTF(TwoFace.String(us("Something")) charAt TwoFace.Int(us(4)), us('t'))

  property("Safe toInt") = verifyTF(TwoFace.String("1").toInt, 1)
  property("Unsafe toInt") = verifyTF(TwoFace.String(us("1")).toInt, us(1))
  property("Safe toLong") = verifyTF(TwoFace.String("1").toLong, 1L)
  property("Unsafe toLong") = verifyTF(TwoFace.String(us("1")).toLong, us(1L))
  property("Safe toFloat") = verifyTF(TwoFace.String("1.0").toFloat, 1.0f)
  property("Unsafe toFloat") = verifyTF(TwoFace.String(us("1.0")).toFloat, us(1.0f))
  property("Safe toDouble") = verifyTF(TwoFace.String("1.0").toDouble, 1.0)
  property("Unsafe toDouble") = verifyTF(TwoFace.String(us("1.0")).toDouble, us(1.0))

  property("Safe length") = verifyTF(TwoFace.String("Some").length, 4)
  property("Unsafe length") = verifyTF(TwoFace.String(us("Some")).length, us(4))

  property("Safe reverse") = verifyTF(TwoFace.String("Some").reverse, "emoS")
  property("Unsafe reverse") = verifyTF(TwoFace.String(us("Some")).reverse, us("emoS"))

  property("Implicit Conversions") = wellTyped {
    val a : TwoFace.String["Something"] = implicitly[TwoFace.String["Some" + "thing"]]
    val b : TwoFace.String["Som" + "ething"] = implicitly[TwoFace.String["Some" + "thing"]]
    val c : TwoFace.String["Some" + "thing"] = implicitly[TwoFace.String["Something"]]
    val d : "Some" = TwoFace.String("Some")
    val e : String = TwoFace.String(us("Some"))
  }

  property("Wrong Implicit Conversions") = {
    illTyped("""val a : TwoFace.String["Some"] = implicitly[TwoFace.String["Som" + "E"]]""")
    illTyped("""val b : TwoFace.String["Some" + "thing"] = implicitly[TwoFace.String["SomeThing"]]""")
    true
  }

  type Fin = "a"
  final val fin = "a"
  property("Extracting from Safe TwoFace") = {
    val a = TwoFace.String(fin)
    val ret = shapeless.the[Id[a.type]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  property("Extracting from Unsafe TwoFace") = wellTyped {
    val a = TwoFace.String(us(fin))
    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
    implicitly[ret.Out =:= String]
    ret.value == fin
  }
}