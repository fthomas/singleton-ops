package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class IdSpec extends Properties("Id") {
  property("Nat") = wellTyped {
    def getNat(implicit op : SafeNat[Id[shapeless.Nat._1]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }
  property("Char") = {
    val ret : Char = implicitly[SafeChar[Id[W.`'\u0001'`.T]]]
    ret == '\u0001'
  }
  property("Int") = {
    val ret : Int = implicitly[SafeInt[Id[W.`1`.T]]]
    ret == 1
  }
  property("Long") = {
    val ret : Long = implicitly[SafeLong[Id[W.`1L`.T]]]
    ret == 1L
  }
  property("Float") = {
    val ret : Float = implicitly[SafeFloat[Id[W.`1.0f`.T]]]
    ret == 1.0f
  }
  property("Double") = {
    val ret : Double = implicitly[SafeDouble[Id[W.`1.0`.T]]]
    ret == 1.0
  }
  property("String") = {
    val ret : String = implicitly[SafeString[Id[W.`"Something"`.T]]]
    ret == "Something"
  }
  property("Boolean") = {
    val ret : Boolean = implicitly[SafeBoolean[Id[W.`true`.T]]]
    ret == true
  }
  property("Symbol") = {
    val ret : Symbol = implicitly[SafeSymbol[Id[W.`"Something"`.T]]]
    ret == 'Something
  }
  property("UpperBound") = {
    trait Foo[T] {
      type Width <: T
      val value : Int
    }
    val ret = new Foo[W.`1`.T]{
      val value : Int = implicitly[SafeInt[Id[Width]]]
    }
    ret.value == 1
  }
}