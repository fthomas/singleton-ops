package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._

class IdSpec extends Properties("Id") {
  property("Nat") = wellTyped {
    def getNat(implicit op : SafeNat[Id[shapeless.Nat._1]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }
  property("Char") = {
    val ret : Char = implicitly[SafeChar[Id['\u0001']]]
    ret == '\u0001'
  }
  property("Int") = {
    val ret : Int = implicitly[SafeInt[Id[1]]]
    ret == 1
  }
  property("Long") = {
    val ret : Long = implicitly[SafeLong[Id[1L]]]
    ret == 1L
  }
  property("Float") = {
    val ret : Float = implicitly[SafeFloat[Id[1.0f]]]
    ret == 1.0f
  }
  property("Double") = {
    val ret : Double = implicitly[SafeDouble[Id[1.0]]]
    ret == 1.0
  }
  property("String") = {
    val ret : String = implicitly[SafeString[Id["Something"]]]
    ret == "Something"
  }
  property("Boolean") = {
    val ret : Boolean = implicitly[SafeBoolean[Id[true]]]
    ret == true
  }
}