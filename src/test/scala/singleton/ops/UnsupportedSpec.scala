package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops.math._

//Unsupported operation check to increase coverage
class UnsupportedSpec extends Properties("UnsupportedSpec") {
  property("ToNat") = wellTyped {illTyped("""implicitly[ToNat[W.`true`.T]]""")}
  property("ToNat with negative number") = wellTyped {illTyped("""implicitly[ToNat[W.`-1`.T]]""","Nat cannot be a negative literal. Found: -1")}
  property("ToChar") = wellTyped {illTyped("""implicitly[ToChar[W.`true`.T]]""")}
  property("ToInt") = wellTyped {illTyped("""implicitly[ToInt[W.`true`.T]]""")}
  property("ToLong") = wellTyped {illTyped("""implicitly[ToLong[W.`true`.T]]""")}
  property("ToFloat") = wellTyped {illTyped("""implicitly[ToFloat[W.`true`.T]]""")}
  property("ToDouble") = wellTyped {illTyped("""implicitly[ToDouble[W.`true`.T]]""")}
  property("Negate") = wellTyped {illTyped("""implicitly[Negate[W.`true`.T]]""")}
  property("Abs") = wellTyped {illTyped("""implicitly[Abs[W.`true`.T]]""")}
  property("ToDouble") = wellTyped {illTyped("""implicitly[ToDouble[W.`true`.T]]""")}
  property("NumberOfLeadingZeros") = wellTyped {illTyped("""implicitly[NumberOfLeadingZeros[W.`true`.T]]""")}
  property("Floor") = wellTyped {illTyped("""implicitly[Floor[W.`true`.T]]""")}
  property("Ceil") = wellTyped {illTyped("""implicitly[Ceil[W.`true`.T]]""")}
  property("Round") = wellTyped {illTyped("""implicitly[Round[W.`true`.T]]""")}
  property("Sin") = wellTyped {illTyped("""implicitly[Sin[W.`true`.T]]""")}
  property("Cos") = wellTyped {illTyped("""implicitly[Cos[W.`true`.T]]""")}
  property("Tan") = wellTyped {illTyped("""implicitly[Tan[W.`true`.T]]""")}
  property("Sqrt") = wellTyped {illTyped("""implicitly[Sqrt[W.`true`.T]]""")}
  property("Log") = wellTyped {illTyped("""implicitly[Log[W.`true`.T]]""")}
  property("Log10") = wellTyped {illTyped("""implicitly[Log10[W.`true`.T]]""")}
  property("Reverse") = wellTyped {illTyped("""implicitly[Reverse[W.`true`.T]]""")}
  property("Not") = wellTyped {illTyped("""implicitly[![W.`"a"`.T]]""")}
  property("Require") = wellTyped {
    val a = W(true)
    illTyped("""implicitly[RequireMsg[W.`false`.T,W.`false`.T]]""")
//    illTyped("""implicitly[RequireMsg[a.T,W.`11`.T]]""")
    illTyped("""implicitly[RequireMsg[W.`1`.T,W.`false`.T]]""")
  }
  property("ITE") = wellTyped {illTyped("""implicitly[ITE[W.`1`.T,W.`1`.T,W.`true`.T]]""")}
  property("+") = wellTyped {illTyped("""implicitly[W.`true`.T + W.`2`.T]""")}
  property("-") = wellTyped {illTyped("""implicitly[W.`"a"`.T - W.`2`.T]""")}
  property("*") = wellTyped {illTyped("""implicitly[W.`true`.T * W.`2`.T]""")}
  property("/") = wellTyped {illTyped("""implicitly[W.`true`.T / W.`2`.T]""")}
  property("%") = wellTyped {illTyped("""implicitly[W.`true`.T % W.`2`.T]""")}
  property("<") = wellTyped {illTyped("""implicitly[W.`true`.T < W.`2`.T]""")}
  property(">") = wellTyped {illTyped("""implicitly[W.`true`.T > W.`2`.T]""")}
  property("<=") = wellTyped {illTyped("""implicitly[W.`true`.T <= W.`2`.T]""")}
  property(">=") = wellTyped {illTyped("""implicitly[W.`true`.T >= W.`2`.T]""")}
  property("==") = wellTyped {illTyped("""implicitly[W.`true`.T == W.`2`.T]""")}
  property("!=") = wellTyped {illTyped("""implicitly[W.`true`.T != W.`2`.T]""")}
  property("&&") = wellTyped {illTyped("""implicitly[W.`1`.T && W.`2`.T]""")}
  property("||") = wellTyped {illTyped("""implicitly[W.`1`.T || W.`2`.T]""")}
  property("Pow") = wellTyped {illTyped("""implicitly[Pow[W.`true`.T, W.`2`.T]]""")}
  property("Min") = wellTyped {illTyped("""implicitly[Min[W.`true`.T, W.`2`.T]]""")}
  property("Max") = wellTyped {illTyped("""implicitly[Max[W.`true`.T, W.`2`.T]]""")}
  property("Substring") = wellTyped {illTyped("""implicitly[Substring[W.`true`.T, W.`2`.T]]""")}
  property("CharAt") = wellTyped {illTyped("""implicitly[CharAt[W.`true`.T, W.`2`.T]]""")}
  property("Length") = wellTyped {illTyped("""implicitly[Length[W.`true`.T]]""")}
  property("No Extraction from Bad Num") = wellTyped {illTyped("""implicitly[Id[Int]]""")}
  property("No Extraction from Bad TwoFace") = wellTyped {illTyped("""implicitly[TwoFace.Int[Int]]""")}
}