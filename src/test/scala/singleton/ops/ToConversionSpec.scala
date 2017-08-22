package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class ToConversionSpec extends Properties("ToConversion") {
  property("Nat to Nat") = wellTyped {
    def getNat(implicit op : SafeNat[ToNat[shapeless.Nat._1]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }
  property("Char to Nat") = wellTyped {
    def getNat(implicit op : SafeNat[ToNat[W.`'\u0001'`.T]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }
  property("Int to Nat") = wellTyped {
    def getNat(implicit op : SafeNat[ToNat[W.`1`.T]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }
  property("Long to Nat") = wellTyped {
    def getNat(implicit op : SafeNat[ToNat[W.`1L`.T]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }
  property("Float to Nat") = wellTyped {
    def getNat(implicit op : SafeNat[ToNat[W.`1.0f`.T]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }
  property("Double to Nat") = wellTyped {
    def getNat(implicit op : SafeNat[ToNat[W.`1.0`.T]]) : op.Out = op.value
    val ret : shapeless.nat._1 = getNat
  }

  property("Nat to Char") = verifyOp1Args[ToChar,shapeless.Nat._1,W.`'\u0001'`.T]
  property("Char to Char") = verifyOp1Args[ToChar,W.`'\u0002'`.T,W.`'\u0002'`.T]
  property("Int to Char") = verifyOp1Args[ToChar,W.`3`.T,W.`'\u0003'`.T]
  property("Long to Char") = verifyOp1Args[ToChar,W.`4L`.T,W.`'\u0004'`.T]
  property("Float to Char") = verifyOp1Args[ToChar,W.`5.0f`.T,W.`'\u0005'`.T]
  property("Double to Char") = verifyOp1Args[ToChar,W.`6.0`.T,W.`'\u0006'`.T]
  property("String to Char") = {illTyped("""implicitly[ToChar[W.`"7"`.T]]"""); true}
  property("Boolean to Char") = {illTyped("""implicitly[ToChar[True]]"""); true}

  property("Nat to Int") = verifyOp1Args[ToInt,shapeless.Nat._1,W.`1`.T]
  property("Char to Int") = verifyOp1Args[ToInt,W.`'\u0002'`.T,W.`2`.T]
  property("Int to Int") = verifyOp1Args[ToInt,W.`3`.T,W.`3`.T]
  property("Long to Int") = verifyOp1Args[ToInt,W.`4L`.T,W.`4`.T]
  property("Float to Int") = verifyOp1Args[ToInt,W.`5.0f`.T,W.`5`.T]
  property("Double to Int") = verifyOp1Args[ToInt,W.`6.0`.T,W.`6`.T]
  property("String to Int") = verifyOp1Args[ToInt,W.`"7"`.T,W.`7`.T]
  property("Boolean to Int") = {illTyped("""implicitly[ToInt[True]]"""); true}

  property("Nat to Long") = verifyOp1Args[ToLong,shapeless.Nat._1,W.`1L`.T]
  property("Char to Long") = verifyOp1Args[ToLong,W.`'\u0002'`.T,W.`2L`.T]
  property("Int to Long") = verifyOp1Args[ToLong,W.`3`.T,W.`3L`.T]
  property("Long to Long") = verifyOp1Args[ToLong,W.`4L`.T,W.`4L`.T]
  property("Float to Long") = verifyOp1Args[ToLong,W.`5.0f`.T,W.`5L`.T]
  property("Double to Long") = verifyOp1Args[ToLong,W.`6.0`.T,W.`6L`.T]
  property("String to Long") = verifyOp1Args[ToLong,W.`"7"`.T,W.`7L`.T]
  property("Boolean to Long") = {illTyped("""implicitly[ToLong[True]]"""); true}

  property("Nat to Float") = verifyOp1Args[ToFloat,shapeless.Nat._1,W.`1.0f`.T]
  property("Char to Float") = verifyOp1Args[ToFloat,W.`'\u0002'`.T,W.`2.0f`.T]
  property("Int to Float") = verifyOp1Args[ToFloat,W.`3`.T,W.`3.0f`.T]
  property("Long to Float") = verifyOp1Args[ToFloat,W.`4L`.T,W.`4.0f`.T]
  property("Float to Float") = verifyOp1Args[ToFloat,W.`5.0f`.T,W.`5.0f`.T]
  property("Double to Float") = verifyOp1Args[ToFloat,W.`6.0`.T,W.`6.0f`.T]
  property("String to Float") = verifyOp1Args[ToFloat,W.`"7"`.T,W.`7.0f`.T]
  property("Boolean to Float") = {illTyped("""implicitly[ToFloat[True]]"""); true}

  property("Nat to Double") = verifyOp1Args[ToDouble,shapeless.Nat._1,W.`1.0`.T]
  property("Char to Double") = verifyOp1Args[ToDouble,W.`'\u0002'`.T,W.`2.0`.T]
  property("Int to Double") = verifyOp1Args[ToDouble,W.`3`.T,W.`3.0`.T]
  property("Long to Double") = verifyOp1Args[ToDouble,W.`4L`.T,W.`4.0`.T]
  property("Float to Double") = verifyOp1Args[ToDouble,W.`5.0f`.T,W.`5.0`.T]
  property("Double to Double") = verifyOp1Args[ToDouble,W.`6.0`.T,W.`6.0`.T]
  property("String to Double") = verifyOp1Args[ToDouble,W.`"7"`.T,W.`7.0`.T]
  property("Boolean to Double") = {illTyped("""implicitly[ToDouble[True]]"""); true}

  property("Nat to String") = verifyOp1Args[ToString,shapeless.Nat._1,W.`"1"`.T]
  property("Char to String") = verifyOp1Args[ToString,W.`'2'`.T,W.`"2"`.T]
  property("Int to String") = verifyOp1Args[ToString,W.`3`.T,W.`"3"`.T]
  property("Long to String") = verifyOp1Args[ToString,W.`4L`.T,W.`"4"`.T]
  property("Float to String") = verifyOp1Args[ToString,W.`5.0f`.T,W.`"5.0"`.T]
  property("Double to String") = verifyOp1Args[ToString,W.`6.0`.T,W.`"6.0"`.T]
  property("String to String") = verifyOp1Args[ToString,W.`"7"`.T,W.`"7"`.T]
  property("Boolean to String") = verifyOp1Args[ToString,True,W.`"true"`.T]

  import shapeless.syntax.singleton._
  val sym = 'foo.narrow
  property("Symbol to String") = verifyOp1Args[ToString,sym.type,W.`"foo"`.T]

}