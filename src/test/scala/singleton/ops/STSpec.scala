package singleton.ops

import org.scalacheck.{Prop, Properties}
import shapeless.test.illTyped
import singleton.TestUtils._

class STSpec extends Properties("<") {
  type OP[L,R] = <[L,R]
  type leftNat = shapeless.Nat._1
  type leftChar = W.`'\u0001'`.T
  type leftInt = W.`1`.T
  type leftLong = W.`1L`.T
  type leftFloat = W.`1.0f`.T
  type leftDouble = W.`1.0`.T
  type leftString = W.`"Something"`.T
  type leftBoolean = True

  type rightNat = shapeless.Nat._2
  type rightChar = W.`'\u0002'`.T
  type rightInt = W.`2`.T
  type rightLong = W.`2L`.T
  type rightFloat = W.`2.0f`.T
  type rightDouble = W.`2.0`.T
  type rightString = W.`"Else"`.T
  type rightBoolean = False

  def verifySTNum[L,R](implicit
                       verifyFalse: Verify[R < L, False],
                       verifyFalse2: Verify[L < L, False],
                       verifyTrue: Verify[L < R, True]) : Prop = wellTyped {}

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifySTNum[leftNat,rightNat]
  property("Nat, Int arguments") = verifySTNum[leftNat,rightInt]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Char arguments") = verifySTNum[leftChar,rightChar]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifySTNum[leftInt,rightNat]
  property("Int, Int arguments") = verifySTNum[leftInt,rightInt]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Long arguments") = verifySTNum[leftLong,rightLong]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Float arguments") = verifySTNum[leftFloat,rightFloat]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Double, Double arguments") = verifySTNum[leftDouble,rightDouble]
  property("Double, String arguments") = {illTyped("""implicitly[OP[leftDouble,rightString]]"""); true}
  property("Double, Boolean arguments") = {illTyped("""implicitly[OP[leftDouble,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // String op XXX
  ////////////////////////////////////////////////////////////////////////
  property("String, Nat arguments") = {illTyped("""implicitly[OP[leftString,rightNat]]"""); true}
  property("String, Char arguments") = {illTyped("""implicitly[OP[leftString,rightChar]]"""); true}
  property("String, Int arguments") = {illTyped("""implicitly[OP[leftString,rightInt]]"""); true}
  property("String, Long arguments") = {illTyped("""implicitly[OP[leftString,rightLong]]"""); true}
  property("String, Float arguments") = {illTyped("""implicitly[OP[leftString,rightFloat]]"""); true}
  property("String, Double arguments") = {illTyped("""implicitly[OP[leftString,rightDouble]]"""); true}
  property("String, String arguments") = {illTyped("""implicitly[OP[leftString,rightString]]"""); true}
  property("String, Boolean arguments") = {illTyped("""implicitly[OP[leftString,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Boolean op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Boolean, Nat arguments") = {illTyped("""implicitly[OP[leftBoolean,rightNat]]"""); true}
  property("Boolean, Char arguments") = {illTyped("""implicitly[OP[leftBoolean,rightChar]]"""); true}
  property("Boolean, Int arguments") = {illTyped("""implicitly[OP[leftBoolean,rightInt]]"""); true}
  property("Boolean, Long arguments") = {illTyped("""implicitly[OP[leftBoolean,rightLong]]"""); true}
  property("Boolean, Float arguments") = {illTyped("""implicitly[OP[leftBoolean,rightFloat]]"""); true}
  property("Boolean, Double arguments") = {illTyped("""implicitly[OP[leftBoolean,rightDouble]]"""); true}
  property("Boolean, String arguments") = {illTyped("""implicitly[OP[leftBoolean,rightString]]"""); true}
  property("Boolean, Boolean arguments") = {illTyped("""implicitly[OP[leftBoolean,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////
}
