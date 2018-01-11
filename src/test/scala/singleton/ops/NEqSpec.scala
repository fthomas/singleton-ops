package singleton.ops

import org.scalacheck.{Prop, Properties}
import shapeless.test.illTyped
import singleton.TestUtils._

class NEqSpec extends Properties("!=") {
  property("Basic boolean arguments") = wellTyped {
    implicitly[Require[False != True]]
    implicitly[Require[True != False]]
  }
  property("Basic boolean arguments") = {
    illTyped("""implicitly[Require[True != True]]""");
    illTyped("""implicitly[Require[False != False]]""");
    true
  }

  type OP[L,R] = !=[L,R]
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

  def verifyNEqNum[L,R](implicit
                        verifyFalse: Verify[L != Negate[R], True],
                        verifyTrue: Verify[L != R, False]) : Prop = wellTyped {}
  def verifyChar[L,R](implicit
                        verifyFalse: Verify[L != ToChar[Negate[R]], True],
                        verifyTrue: Verify[L != R, False]) : Prop = wellTyped {}

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifyNEqNum[leftNat,rightNat]
  property("Nat, Int arguments") = verifyNEqNum[leftNat,rightInt]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Char arguments") = verifyChar[leftChar,rightChar]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifyNEqNum[leftInt,rightNat]
  property("Int, Int arguments") = verifyNEqNum[leftInt,rightInt]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Long arguments") = verifyNEqNum[leftLong,rightLong]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Float arguments") = verifyNEqNum[leftFloat,rightFloat]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
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
    verifyOp2Args[OP,leftString,leftString,False]
    verifyOp2Args[OP,leftString,rightString,True]
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
