package singleton.ops

import org.scalacheck.{Prop, Properties}
import shapeless.test.illTyped
import singleton.TestUtils._

class BTESpec extends Properties(">=") {
  type OP[L,R] = >=[L,R]
  type leftNat = shapeless.Nat._3
  type leftChar = '\u0003'
  type leftInt = 3
  type leftLong = 3L
  type leftFloat = 3.0f
  type leftDouble = 3.0
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

  def verifyBTENum[L,R](implicit
                        verifyFalse: Verify[Negate[L] >= R, resultFalse],
                        verifyTrue: Verify[L >= L, resultTrue],
                        verifyTrue2: Verify[L >= R, resultTrue]) : Prop = wellTyped {}

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifyBTENum[leftNat,rightNat]
  property("Nat, Char arguments") = verifyBTENum[leftNat,rightChar]
  property("Nat, Int arguments") = verifyBTENum[leftNat,rightInt]
  property("Nat, Long arguments") = verifyBTENum[leftNat,rightLong]
  property("Nat, Float arguments") = verifyBTENum[leftNat,rightFloat]
  property("Nat, Double arguments") = verifyBTENum[leftNat,rightDouble]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Nat arguments") = verifyBTENum[leftChar,rightNat]
  property("Char, Char arguments") = verifyBTENum[leftChar,rightChar]
  property("Char, Int arguments") = verifyBTENum[leftChar,rightInt]
  property("Char, Long arguments") = verifyBTENum[leftChar,rightLong]
  property("Char, Float arguments") = verifyBTENum[leftChar,rightFloat]
  property("Char, Double arguments") = verifyBTENum[leftChar,rightDouble]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifyBTENum[leftInt,rightNat]
  property("Int, Char arguments") = verifyBTENum[leftInt,rightChar]
  property("Int, Int arguments") = verifyBTENum[leftInt,rightInt]
  property("Int, Long arguments") = verifyBTENum[leftInt,rightLong]
  property("Int, Float arguments") = verifyBTENum[leftInt,rightFloat]
  property("Int, Double arguments") = verifyBTENum[leftInt,rightDouble]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Nat arguments") = verifyBTENum[leftLong,rightNat]
  property("Long, Char arguments") = verifyBTENum[leftLong,rightChar]
  property("Long, Int arguments") = verifyBTENum[leftLong,rightInt]
  property("Long, Long arguments") = verifyBTENum[leftLong,rightLong]
  property("Long, Float arguments") = verifyBTENum[leftLong,rightFloat]
  property("Long, Double arguments") = verifyBTENum[leftLong,rightDouble]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Nat arguments") = verifyBTENum[leftFloat,rightNat]
  property("Float, Char arguments") = verifyBTENum[leftFloat,rightChar]
  property("Float, Int arguments") = verifyBTENum[leftFloat,rightInt]
  property("Float, Long arguments") = verifyBTENum[leftFloat,rightLong]
  property("Float, Float arguments") = verifyBTENum[leftFloat,rightFloat]
  property("Float, Double arguments") = verifyBTENum[leftFloat,rightDouble]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Double, Nat arguments") = verifyBTENum[leftDouble,rightNat]
  property("Double, Char arguments") = verifyBTENum[leftDouble,rightChar]
  property("Double, Int arguments") = verifyBTENum[leftDouble,rightInt]
  property("Double, Long arguments") = verifyBTENum[leftDouble,rightLong]
  property("Double, Float arguments") = verifyBTENum[leftDouble,rightFloat]
  property("Double, Double arguments") = verifyBTENum[leftDouble,rightDouble]
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
