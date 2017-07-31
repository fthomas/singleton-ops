package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops.math._

//Unsupported operation check to increase coverage
class UnsupportedSpec extends Properties("UnsupportedSpec") {
  property("ToNat") = wellTyped {illTyped("""implicitly[ToNat[true]]""")}
  property("ToNat with negative number") = wellTyped {illTyped("""implicitly[ToNat[-1]]""")}
  property("ToChar") = wellTyped {illTyped("""implicitly[ToChar[true]]""")}
  property("ToInt") = wellTyped {illTyped("""implicitly[ToInt[true]]""")}
  property("ToLong") = wellTyped {illTyped("""implicitly[ToLong[true]]""")}
  property("ToFloat") = wellTyped {illTyped("""implicitly[ToFloat[true]]""")}
  property("ToDouble") = wellTyped {illTyped("""implicitly[ToDouble[true]]""")}
  property("Negate") = wellTyped {illTyped("""implicitly[Negate[true]]""")}
  property("Abs") = wellTyped {illTyped("""implicitly[Abs[true]]""")}
  property("ToDouble") = wellTyped {illTyped("""implicitly[ToDouble[true]]""")}
  property("NumberOfLeadingZeros") = wellTyped {illTyped("""implicitly[NumberOfLeadingZeros[true]]""")}
  property("Floor") = wellTyped {illTyped("""implicitly[Floor[true]]""")}
  property("Ceil") = wellTyped {illTyped("""implicitly[Ceil[true]]""")}
  property("Round") = wellTyped {illTyped("""implicitly[Round[true]]""")}
  property("Sin") = wellTyped {illTyped("""implicitly[Sin[true]]""")}
  property("Cos") = wellTyped {illTyped("""implicitly[Cos[true]]""")}
  property("Tan") = wellTyped {illTyped("""implicitly[Tan[true]]""")}
  property("Sqrt") = wellTyped {illTyped("""implicitly[Sqrt[true]]""")}
  property("Log") = wellTyped {illTyped("""implicitly[Log[true]]""")}
  property("Log10") = wellTyped {illTyped("""implicitly[Log10[true]]""")}
  property("Reverse") = wellTyped {illTyped("""implicitly[Reverse[true]]""")}
  property("Not") = wellTyped {illTyped("""implicitly[Not["a"]]""")}
  property("Require") = wellTyped {
    val a = true
    illTyped("""implicitly[Require[false,false]]""")
    illTyped("""implicitly[Require[a.type,11]]""")
    illTyped("""implicitly[Require[1,false]]""")
  }
  property("ITE") = wellTyped {illTyped("""implicitly[ITE[1,1,true]]""")}
  property("+") = wellTyped {illTyped("""implicitly[true + 2]""")}
  property("-") = wellTyped {illTyped("""implicitly["a" - 2]""")}
  property("*") = wellTyped {illTyped("""implicitly[true * 2]""")}
  property("/") = wellTyped {illTyped("""implicitly[true / 2]""")}
  property("%") = wellTyped {illTyped("""implicitly[true % 2]""")}
  property("<") = wellTyped {illTyped("""implicitly[true < 2]""")}
  property(">") = wellTyped {illTyped("""implicitly[true > 2]""")}
  property("<=") = wellTyped {illTyped("""implicitly[true <= 2]""")}
  property(">=") = wellTyped {illTyped("""implicitly[true >= 2]""")}
  property("==") = wellTyped {illTyped("""implicitly[true == 2]""")}
  property("!=") = wellTyped {illTyped("""implicitly[true != 2]""")}
  property("&&") = wellTyped {illTyped("""implicitly[1 && 2]""")}
  property("||") = wellTyped {illTyped("""implicitly[1 || 2]""")}
  property("Pow") = wellTyped {illTyped("""implicitly[Pow[true, 2]]""")}
  property("Min") = wellTyped {illTyped("""implicitly[Min[true, 2]]""")}
  property("Max") = wellTyped {illTyped("""implicitly[Max[true, 2]]""")}
  property("Substring") = wellTyped {illTyped("""implicitly[Substring[true, 2]]""")}
  property("CharAt") = wellTyped {illTyped("""implicitly[CharAt[true, 2]]""")}
  property("Length") = wellTyped {illTyped("""implicitly[Length[true]]""")}
}