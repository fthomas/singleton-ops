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
  type resultString = "SomethingElse"
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

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Nat arguments") = verifyOpArgs[OP,leftLong,rightNat,resultLong]
  property("Long, Char arguments") = verifyOpArgs[OP,leftLong,rightChar,resultLong]
  property("Long, Int arguments") = verifyOpArgs[OP,leftLong,rightInt,resultLong]
  property("Long, Long arguments") = verifyOpArgs[OP,leftLong,rightLong,resultLong]
  property("Long, Float arguments") = verifyOpArgs[OP,leftLong,rightFloat,resultFloat]
  property("Long, Double arguments") = verifyOpArgs[OP,leftLong,rightDouble,resultDouble]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Nat arguments") = verifyOpArgs[OP,leftFloat,rightNat,resultFloat]
  property("Float, Char arguments") = verifyOpArgs[OP,leftFloat,rightChar,resultFloat]
  property("Float, Int arguments") = verifyOpArgs[OP,leftFloat,rightInt,resultFloat]
  property("Float, Long arguments") = verifyOpArgs[OP,leftFloat,rightLong,resultFloat]
  property("Float, Float arguments") = verifyOpArgs[OP,leftFloat,rightFloat,resultFloat]
  property("Float, Double arguments") = verifyOpArgs[OP,leftFloat,rightDouble,resultDouble]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Double, Nat arguments") = verifyOpArgs[OP,leftDouble,rightNat,resultDouble]
  property("Double, Char arguments") = verifyOpArgs[OP,leftDouble,rightChar,resultDouble]
  property("Double, Int arguments") = verifyOpArgs[OP,leftDouble,rightInt,resultDouble]
  property("Double, Long arguments") = verifyOpArgs[OP,leftDouble,rightLong,resultDouble]
  property("Double, Float arguments") = verifyOpArgs[OP,leftDouble,rightFloat,resultDouble]
  property("Double, Double arguments") = verifyOpArgs[OP,leftDouble,rightDouble,resultDouble]
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
  property("String, String arguments") = verifyOpArgs[OP,leftString,rightString,resultString]
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
