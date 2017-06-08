package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class PlusSpec extends Properties("+") {
  type OP[L,R] = +[L,R]
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

  type resultNat = shapeless.Nat._3
  type resultChar = '\u0003'
  type resultInt = 3
  type resultLong = 3L
  type resultFloat = 3.0f
  type resultDouble = 3.0
  type resultString = "Else"
  type resultBoolean = false

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifyOpArgs[OP,leftNat,rightNat,resultInt]
  property("Nat, Char arguments") = verifyOpArgs[OP,leftNat,rightChar,resultInt]
  property("Nat, Int arguments") = verifyOpArgs[OP,leftNat,rightInt,resultInt]
  property("Nat, Long arguments") = verifyOpArgs[OP,leftNat,rightLong,resultLong]
  property("Nat, Float arguments") = verifyOpArgs[OP,leftNat,rightFloat,resultFloat]
  property("Nat, Double arguments") = verifyOpArgs[OP,leftNat,rightDouble,resultDouble]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Nat arguments") = verifyOpArgs[OP,leftChar,rightNat,resultInt]
  property("Char, Char arguments") = verifyOpArgs[OP,leftChar,rightChar,resultInt]
  property("Char, Int arguments") = verifyOpArgs[OP,leftChar,rightInt,resultInt]
  property("Char, Long arguments") = verifyOpArgs[OP,leftChar,rightLong,resultLong]
  property("Char, Float arguments") = verifyOpArgs[OP,leftChar,rightFloat,resultFloat]
  property("Char, Double arguments") = verifyOpArgs[OP,leftChar,rightDouble,resultDouble]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifyOpArgs[OP,leftInt,rightNat,resultInt]
  property("Int, Char arguments") = verifyOpArgs[OP,leftInt,rightChar,resultInt]
  property("Int, Int arguments") = verifyOpArgs[OP,leftInt,rightInt,resultInt]
  property("Int, Long arguments") = verifyOpArgs[OP,leftInt,rightLong,resultLong]
  property("Int, Float arguments") = verifyOpArgs[OP,leftInt,rightFloat,resultFloat]
  property("Int, Double arguments") = verifyOpArgs[OP,leftInt,rightDouble,resultDouble]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////
}
