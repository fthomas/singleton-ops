package singleton.ops

import org.scalacheck.{Prop, Properties}
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class STESpec extends Properties("<=") {
  type OP[L,R] = <=[L,R]
  type leftNat = shapeless.Nat._1
  type leftChar = '\u0001'
  type leftInt = 1
  type leftLong = 1L
  type leftFloat = 1.0f
  type leftDouble = 1.0
  type leftString = "Something"
  type leftBoolean = true

  type rightNat = shapeless.Nat._2
  type rightChar = '\u0002'
  type rightInt = 2
  type rightLong = 2L
  type rightFloat = 2.0f
  type rightDouble = 2.0
  type rightString = "Else"
  type rightBoolean = false

  type resultFalse = false
  type resultTrue = true

  def verifySTENum[L,R](implicit
                        verifyFalse: Verify[L <= Negate[R], resultFalse],
                        verifyTrue: Verify[L <= L, resultTrue],
                        verifyTrue2: Verify[L <= R, resultTrue]) : Prop = wellTyped {}

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifySTENum[leftNat,rightNat]
  property("Nat, Char arguments") = verifySTENum[leftNat,rightChar]
  property("Nat, Int arguments") = verifySTENum[leftNat,rightInt]
  property("Nat, Long arguments") = verifySTENum[leftNat,rightLong]
  property("Nat, Float arguments") = verifySTENum[leftNat,rightFloat]
  property("Nat, Double arguments") = verifySTENum[leftNat,rightDouble]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Nat arguments") = verifySTENum[leftChar,rightNat]
  property("Char, Char arguments") = verifySTENum[leftChar,rightChar]
  property("Char, Int arguments") = verifySTENum[leftChar,rightInt]
  property("Char, Long arguments") = verifySTENum[leftChar,rightLong]
  property("Char, Float arguments") = verifySTENum[leftChar,rightFloat]
  property("Char, Double arguments") = verifySTENum[leftChar,rightDouble]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifySTENum[leftInt,rightNat]
  property("Int, Char arguments") = verifySTENum[leftInt,rightChar]
  property("Int, Int arguments") = verifySTENum[leftInt,rightInt]
  property("Int, Long arguments") = verifySTENum[leftInt,rightLong]
  property("Int, Float arguments") = verifySTENum[leftInt,rightFloat]
  property("Int, Double arguments") = verifySTENum[leftInt,rightDouble]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Nat arguments") = verifySTENum[leftLong,rightNat]
  property("Long, Char arguments") = verifySTENum[leftLong,rightChar]
  property("Long, Int arguments") = verifySTENum[leftLong,rightInt]
  property("Long, Long arguments") = verifySTENum[leftLong,rightLong]
  property("Long, Float arguments") = verifySTENum[leftLong,rightFloat]
  property("Long, Double arguments") = verifySTENum[leftLong,rightDouble]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Nat arguments") = verifySTENum[leftFloat,rightNat]
  property("Float, Char arguments") = verifySTENum[leftFloat,rightChar]
  property("Float, Int arguments") = verifySTENum[leftFloat,rightInt]
  property("Float, Long arguments") = verifySTENum[leftFloat,rightLong]
  property("Float, Float arguments") = verifySTENum[leftFloat,rightFloat]
  property("Float, Double arguments") = verifySTENum[leftFloat,rightDouble]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Double, Nat arguments") = verifySTENum[leftDouble,rightNat]
  property("Double, Char arguments") = verifySTENum[leftDouble,rightChar]
  property("Double, Int arguments") = verifySTENum[leftDouble,rightInt]
  property("Double, Long arguments") = verifySTENum[leftDouble,rightLong]
  property("Double, Float arguments") = verifySTENum[leftDouble,rightFloat]
  property("Double, Double arguments") = verifySTENum[leftDouble,rightDouble]
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
