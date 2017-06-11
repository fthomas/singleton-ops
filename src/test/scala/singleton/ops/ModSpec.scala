package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.ops.TestUtils._

class ModSpec extends Properties("%") {
  type OP[L,R] = %[L,R]
  type leftNat = shapeless.Nat._6
  type leftChar = '\u0006'
  type leftInt = 6
  type leftLong = 6L
  type leftFloat = 6.0f
  type leftDouble = 6.0
  type leftString = "Something"
  type leftBoolean = true

  type rightNat = shapeless.Nat._7
  type rightChar = '\u0007'
  type rightInt = 7
  type rightLong = 7L
  type rightFloat = 7.0f
  type rightDouble = 7.0
  type rightString = "Else"
  type rightBoolean = false

  type resultInt = 6
  type resultLong = 6L
  type resultFloat = 6.0f
  type resultDouble = 6.0

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifyOp2Args[OP,leftNat,rightNat,resultInt]
  property("Nat, Char arguments") = verifyOp2Args[OP,leftNat,rightChar,resultInt]
  property("Nat, Int arguments") = verifyOp2Args[OP,leftNat,rightInt,resultInt]
  property("Nat, Long arguments") = verifyOp2Args[OP,leftNat,rightLong,resultLong]
  property("Nat, Float arguments") = verifyOp2Args[OP,leftNat,rightFloat,resultFloat]
  property("Nat, Double arguments") = verifyOp2Args[OP,leftNat,rightDouble,resultDouble]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Nat arguments") = verifyOp2Args[OP,leftChar,rightNat,resultInt]
  property("Char, Char arguments") = verifyOp2Args[OP,leftChar,rightChar,resultInt]
  property("Char, Int arguments") = verifyOp2Args[OP,leftChar,rightInt,resultInt]
  property("Char, Long arguments") = verifyOp2Args[OP,leftChar,rightLong,resultLong]
  property("Char, Float arguments") = verifyOp2Args[OP,leftChar,rightFloat,resultFloat]
  property("Char, Double arguments") = verifyOp2Args[OP,leftChar,rightDouble,resultDouble]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifyOp2Args[OP,leftInt,rightNat,resultInt]
  property("Int, Char arguments") = verifyOp2Args[OP,leftInt,rightChar,resultInt]
  property("Int, Int arguments") = verifyOp2Args[OP,leftInt,rightInt,resultInt]
  property("Int, Long arguments") = verifyOp2Args[OP,leftInt,rightLong,resultLong]
  property("Int, Float arguments") = verifyOp2Args[OP,leftInt,rightFloat,resultFloat]
  property("Int, Double arguments") = verifyOp2Args[OP,leftInt,rightDouble,resultDouble]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Nat arguments") = verifyOp2Args[OP,leftLong,rightNat,resultLong]
  property("Long, Char arguments") = verifyOp2Args[OP,leftLong,rightChar,resultLong]
  property("Long, Int arguments") = verifyOp2Args[OP,leftLong,rightInt,resultLong]
  property("Long, Long arguments") = verifyOp2Args[OP,leftLong,rightLong,resultLong]
  property("Long, Float arguments") = verifyOp2Args[OP,leftLong,rightFloat,resultFloat]
  property("Long, Double arguments") = verifyOp2Args[OP,leftLong,rightDouble,resultDouble]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Nat arguments") = verifyOp2Args[OP,leftFloat,rightNat,resultFloat]
  property("Float, Char arguments") = verifyOp2Args[OP,leftFloat,rightChar,resultFloat]
  property("Float, Int arguments") = verifyOp2Args[OP,leftFloat,rightInt,resultFloat]
  property("Float, Long arguments") = verifyOp2Args[OP,leftFloat,rightLong,resultFloat]
  property("Float, Float arguments") = verifyOp2Args[OP,leftFloat,rightFloat,resultFloat]
  property("Float, Double arguments") = verifyOp2Args[OP,leftFloat,rightDouble,resultDouble]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Double, Nat arguments") = verifyOp2Args[OP,leftDouble,rightNat,resultDouble]
  property("Double, Char arguments") = verifyOp2Args[OP,leftDouble,rightChar,resultDouble]
  property("Double, Int arguments") = verifyOp2Args[OP,leftDouble,rightInt,resultDouble]
  property("Double, Long arguments") = verifyOp2Args[OP,leftDouble,rightLong,resultDouble]
  property("Double, Float arguments") = verifyOp2Args[OP,leftDouble,rightFloat,resultDouble]
  property("Double, Double arguments") = verifyOp2Args[OP,leftDouble,rightDouble,resultDouble]
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
