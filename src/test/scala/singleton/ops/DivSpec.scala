package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class DivSpec extends Properties("/") {
  type OP[L,R] = /[L,R]
  type leftNat = shapeless.Nat._12
  type leftChar = W.`'\u000C'`.T
  type leftInt = W.`12`.T
  type leftLong = W.`12L`.T
  type leftFloat = W.`12.0f`.T
  type leftDouble = W.`12.0`.T
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

  type resultInt = W.`6`.T
  type resultLong = W.`6L`.T
  type resultFloat = W.`6.0f`.T
  type resultDouble = W.`6.0`.T

  ////////////////////////////////////////////////////////////////////////
  // Nat op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Nat, Nat arguments") = verifyOp2Args[OP,leftNat,rightNat,resultInt]
  property("Nat, Int arguments") = verifyOp2Args[OP,leftNat,rightInt,resultInt]
  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Char op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Char, Char arguments") = verifyOp2Args[OP,leftChar,rightChar,resultInt]
  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Int op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Int, Nat arguments") = verifyOp2Args[OP,leftInt,rightNat,resultInt]
  property("Int, Int arguments") = verifyOp2Args[OP,leftInt,rightInt,resultInt]
  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Long op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Long, Long arguments") = verifyOp2Args[OP,leftLong,rightLong,resultLong]
  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Float op XXX
  ////////////////////////////////////////////////////////////////////////
  property("Float, Float arguments") = verifyOp2Args[OP,leftFloat,rightFloat,resultFloat]
  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Double op XXX
  ////////////////////////////////////////////////////////////////////////
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
