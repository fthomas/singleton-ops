package singleton.ops

import org.scalacheck.{Prop, Properties}
import shapeless.test.illTyped
import singleton.TestUtils._

class NEqSpec extends Properties("!=") {
  property("Basic boolean arguments") = wellTyped {
    implicitly[Require[false != true]]
    implicitly[Require[true != false]]
  }
  property("Basic boolean arguments") = {
    illTyped("""implicitly[Require[true != true]]""");
    illTyped("""implicitly[Require[false != false]]""");
    true
  }

  type OP[L,R] = !=[L,R]
  type leftNat = shapeless.Nat._1
  type leftChar = '\u0001'
  type leftInt = 1
  type leftLong = 1L
  type leftFloat = 1.0f
  type leftDouble = 1.0
  type leftString = "Something"
  type leftBoolean = true

  type rightNat = shapeless.Nat._1
  type rightChar = '\u0001'
  type rightInt = 1
  type rightLong = 1L
  type rightFloat = 1.0f
  type rightDouble = 1.0
  type rightString = "Else"
  type rightBoolean = false

  type resultFalse = false
  type resultTrue = true

  def verifyNEqNum[L,R](implicit
                        verifyFalse: Verify[L != Negate[R], resultTrue],
                        verifyTrue: Verify[L != R, resultFalse]) : Prop = wellTyped {}

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifyNEqNum[leftNat,rightNat]
  property("Nat, Char arguments") = verifyNEqNum[leftNat,rightChar]
  property("Nat, Int arguments") = verifyNEqNum[leftNat,rightInt]
  property("Nat, Long arguments") = verifyNEqNum[leftNat,rightLong]
  property("Nat, Float arguments") = verifyNEqNum[leftNat,rightFloat]
  property("Nat, Double arguments") = verifyNEqNum[leftNat,rightDouble]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Nat arguments") = verifyNEqNum[leftChar,rightNat]
  property("Char, Char arguments") = verifyNEqNum[leftChar,rightChar]
  property("Char, Int arguments") = verifyNEqNum[leftChar,rightInt]
  property("Char, Long arguments") = verifyNEqNum[leftChar,rightLong]
  property("Char, Float arguments") = verifyNEqNum[leftChar,rightFloat]
  property("Char, Double arguments") = verifyNEqNum[leftChar,rightDouble]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifyNEqNum[leftInt,rightNat]
  property("Int, Char arguments") = verifyNEqNum[leftInt,rightChar]
  property("Int, Int arguments") = verifyNEqNum[leftInt,rightInt]
  property("Int, Long arguments") = verifyNEqNum[leftInt,rightLong]
  property("Int, Float arguments") = verifyNEqNum[leftInt,rightFloat]
  property("Int, Double arguments") = verifyNEqNum[leftInt,rightDouble]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Nat arguments") = verifyNEqNum[leftLong,rightNat]
  property("Long, Char arguments") = verifyNEqNum[leftLong,rightChar]
  property("Long, Int arguments") = verifyNEqNum[leftLong,rightInt]
  property("Long, Long arguments") = verifyNEqNum[leftLong,rightLong]
  property("Long, Float arguments") = verifyNEqNum[leftLong,rightFloat]
  property("Long, Double arguments") = verifyNEqNum[leftLong,rightDouble]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Nat arguments") = verifyNEqNum[leftFloat,rightNat]
  property("Float, Char arguments") = verifyNEqNum[leftFloat,rightChar]
  property("Float, Int arguments") = verifyNEqNum[leftFloat,rightInt]
  property("Float, Long arguments") = verifyNEqNum[leftFloat,rightLong]
  property("Float, Float arguments") = verifyNEqNum[leftFloat,rightFloat]
  property("Float, Double arguments") = verifyNEqNum[leftFloat,rightDouble]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Double, Nat arguments") = verifyNEqNum[leftDouble,rightNat]
  property("Double, Char arguments") = verifyNEqNum[leftDouble,rightChar]
  property("Double, Int arguments") = verifyNEqNum[leftDouble,rightInt]
  property("Double, Long arguments") = verifyNEqNum[leftDouble,rightLong]
  property("Double, Float arguments") = verifyNEqNum[leftDouble,rightFloat]
  property("Double, Double arguments") = verifyNEqNum[leftDouble,rightDouble]
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
  property("String, String arguments") = {
    verifyOp2Args[OP,leftString,leftString,resultFalse]
    verifyOp2Args[OP,leftString,rightString,resultTrue]
  }
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
  ////////////////////////////////////////////////////////////////////////
}
