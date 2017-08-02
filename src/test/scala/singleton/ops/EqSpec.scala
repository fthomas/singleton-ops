package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import org.scalacheck.Prop

class EqSpec extends Properties("==") {
  property("Basic boolean arguments") = wellTyped {
    implicitly[Require[True == True]]
    implicitly[Require[False == False]]
  }
  property("Basic boolean arguments") = {
    illTyped("""implicitly[Require[True == False]]""");
    illTyped("""implicitly[Require[False == True]]""");
    true
  }

  type OP[L,R] = ==[L,R]
  type leftNat = shapeless.Nat._1
  type leftChar = W.`'\u0001'`.T
  type leftInt = W.`1`.T
  type leftLong = W.`1L`.T
  type leftFloat = W.`1.0f`.T
  type leftDouble = W.`1.0`.T
  type leftString = W.`"Something"`.T
  type leftBoolean = True

  type rightNat = shapeless.Nat._1
  type rightChar = W.`'\u0001'`.T
  type rightInt = W.`1`.T
  type rightLong = W.`1L`.T
  type rightFloat = W.`1.0f`.T
  type rightDouble = W.`1.0`.T
  type rightString = W.`"Else"`.T
  type rightBoolean = False

  def verifyEqNum[L,R](implicit
                       verifyFalse: Verify[L == Negate[R], False],
                       verifyTrue: Verify[L == R, True]) : Prop = wellTyped {}

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifyEqNum[leftNat,rightNat]
  property("Nat, Char arguments") = verifyEqNum[leftNat,rightChar]
  property("Nat, Int arguments") = verifyEqNum[leftNat,rightInt]
  property("Nat, Long arguments") = verifyEqNum[leftNat,rightLong]
  property("Nat, Float arguments") = verifyEqNum[leftNat,rightFloat]
  property("Nat, Double arguments") = verifyEqNum[leftNat,rightDouble]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Nat arguments") = verifyEqNum[leftChar,rightNat]
  property("Char, Char arguments") = verifyEqNum[leftChar,rightChar]
  property("Char, Int arguments") = verifyEqNum[leftChar,rightInt]
  property("Char, Long arguments") = verifyEqNum[leftChar,rightLong]
  property("Char, Float arguments") = verifyEqNum[leftChar,rightFloat]
  property("Char, Double arguments") = verifyEqNum[leftChar,rightDouble]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifyEqNum[leftInt,rightNat]
  property("Int, Char arguments") = verifyEqNum[leftInt,rightChar]
  property("Int, Int arguments") = verifyEqNum[leftInt,rightInt]
  property("Int, Long arguments") = verifyEqNum[leftInt,rightLong]
  property("Int, Float arguments") = verifyEqNum[leftInt,rightFloat]
  property("Int, Double arguments") = verifyEqNum[leftInt,rightDouble]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Nat arguments") = verifyEqNum[leftLong,rightNat]
  property("Long, Char arguments") = verifyEqNum[leftLong,rightChar]
  property("Long, Int arguments") = verifyEqNum[leftLong,rightInt]
  property("Long, Long arguments") = verifyEqNum[leftLong,rightLong]
  property("Long, Float arguments") = verifyEqNum[leftLong,rightFloat]
  property("Long, Double arguments") = verifyEqNum[leftLong,rightDouble]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Nat arguments") = verifyEqNum[leftFloat,rightNat]
  property("Float, Char arguments") = verifyEqNum[leftFloat,rightChar]
  property("Float, Int arguments") = verifyEqNum[leftFloat,rightInt]
  property("Float, Long arguments") = verifyEqNum[leftFloat,rightLong]
  property("Float, Float arguments") = verifyEqNum[leftFloat,rightFloat]
  property("Float, Double arguments") = verifyEqNum[leftFloat,rightDouble]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Double, Nat arguments") = verifyEqNum[leftDouble,rightNat]
  property("Double, Char arguments") = verifyEqNum[leftDouble,rightChar]
  property("Double, Int arguments") = verifyEqNum[leftDouble,rightInt]
  property("Double, Long arguments") = verifyEqNum[leftDouble,rightLong]
  property("Double, Float arguments") = verifyEqNum[leftDouble,rightFloat]
  property("Double, Double arguments") = verifyEqNum[leftDouble,rightDouble]
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
    verifyOp2Args[OP,leftString,leftString,True]
    verifyOp2Args[OP,leftString,rightString,False]
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
