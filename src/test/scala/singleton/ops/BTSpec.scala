//package singleton.ops
//
//import org.scalacheck.{Prop, Properties}
//import shapeless.test.illTyped
//import singleton.TestUtils._
//
//class BTSpec extends Properties(">") {
//  type OP[L,R] = >[L,R]
//  type leftNat = shapeless.Nat._3
//  type leftChar = '\u0003'
//  type leftInt = 3
//  type leftLong = 3L
//  type leftFloat = 3.0f
//  type leftDouble = 3.0
//  type leftString = "Something"
//  type leftBoolean = true
//
//  type rightNat = shapeless.Nat._2
//  type rightChar = '\u0002'
//  type rightInt = 2
//  type rightLong = 2L
//  type rightFloat = 2.0f
//  type rightDouble = 2.0
//  type rightString = "Else"
//  type rightBoolean = false
//
//  type resultFalse = false
//  type resultTrue = true
//
//  def verifyBTNum[L,R](implicit
//                       verifyFalse: Verify[Negate[L] > R, resultFalse],
//                       verifyFalse2: Verify[L > L, resultFalse],
//                       verifyTrue: Verify[L > R, resultTrue]) : Prop = wellTyped {}
//
//  ////////////////////////////////////////////////////////////////////////
//  // Nat op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Nat, Nat arguments") = verifyBTNum[leftNat,rightNat]
//  property("Nat, Char arguments") = verifyBTNum[leftNat,rightChar]
//  property("Nat, Int arguments") = verifyBTNum[leftNat,rightInt]
//  property("Nat, Long arguments") = verifyBTNum[leftNat,rightLong]
//  property("Nat, Float arguments") = verifyBTNum[leftNat,rightFloat]
//  property("Nat, Double arguments") = verifyBTNum[leftNat,rightDouble]
//  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
//  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Char op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Char, Nat arguments") = verifyBTNum[leftChar,rightNat]
//  property("Char, Char arguments") = verifyBTNum[leftChar,rightChar]
//  property("Char, Int arguments") = verifyBTNum[leftChar,rightInt]
//  property("Char, Long arguments") = verifyBTNum[leftChar,rightLong]
//  property("Char, Float arguments") = verifyBTNum[leftChar,rightFloat]
//  property("Char, Double arguments") = verifyBTNum[leftChar,rightDouble]
//  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
//  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Int op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Int, Nat arguments") = verifyBTNum[leftInt,rightNat]
//  property("Int, Char arguments") = verifyBTNum[leftInt,rightChar]
//  property("Int, Int arguments") = verifyBTNum[leftInt,rightInt]
//  property("Int, Long arguments") = verifyBTNum[leftInt,rightLong]
//  property("Int, Float arguments") = verifyBTNum[leftInt,rightFloat]
//  property("Int, Double arguments") = verifyBTNum[leftInt,rightDouble]
//  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
//  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Long op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Long, Nat arguments") = verifyBTNum[leftLong,rightNat]
//  property("Long, Char arguments") = verifyBTNum[leftLong,rightChar]
//  property("Long, Int arguments") = verifyBTNum[leftLong,rightInt]
//  property("Long, Long arguments") = verifyBTNum[leftLong,rightLong]
//  property("Long, Float arguments") = verifyBTNum[leftLong,rightFloat]
//  property("Long, Double arguments") = verifyBTNum[leftLong,rightDouble]
//  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
//  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Float op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Float, Nat arguments") = verifyBTNum[leftFloat,rightNat]
//  property("Float, Char arguments") = verifyBTNum[leftFloat,rightChar]
//  property("Float, Int arguments") = verifyBTNum[leftFloat,rightInt]
//  property("Float, Long arguments") = verifyBTNum[leftFloat,rightLong]
//  property("Float, Float arguments") = verifyBTNum[leftFloat,rightFloat]
//  property("Float, Double arguments") = verifyBTNum[leftFloat,rightDouble]
//  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
//  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Double op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Double, Nat arguments") = verifyBTNum[leftDouble,rightNat]
//  property("Double, Char arguments") = verifyBTNum[leftDouble,rightChar]
//  property("Double, Int arguments") = verifyBTNum[leftDouble,rightInt]
//  property("Double, Long arguments") = verifyBTNum[leftDouble,rightLong]
//  property("Double, Float arguments") = verifyBTNum[leftDouble,rightFloat]
//  property("Double, Double arguments") = verifyBTNum[leftDouble,rightDouble]
//  property("Double, String arguments") = {illTyped("""implicitly[OP[leftDouble,rightString]]"""); true}
//  property("Double, Boolean arguments") = {illTyped("""implicitly[OP[leftDouble,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // String op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("String, Nat arguments") = {illTyped("""implicitly[OP[leftString,rightNat]]"""); true}
//  property("String, Char arguments") = {illTyped("""implicitly[OP[leftString,rightChar]]"""); true}
//  property("String, Int arguments") = {illTyped("""implicitly[OP[leftString,rightInt]]"""); true}
//  property("String, Long arguments") = {illTyped("""implicitly[OP[leftString,rightLong]]"""); true}
//  property("String, Float arguments") = {illTyped("""implicitly[OP[leftString,rightFloat]]"""); true}
//  property("String, Double arguments") = {illTyped("""implicitly[OP[leftString,rightDouble]]"""); true}
//  property("String, String arguments") = {illTyped("""implicitly[OP[leftString,rightString]]"""); true}
//  property("String, Boolean arguments") = {illTyped("""implicitly[OP[leftString,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Boolean op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Boolean, Nat arguments") = {illTyped("""implicitly[OP[leftBoolean,rightNat]]"""); true}
//  property("Boolean, Char arguments") = {illTyped("""implicitly[OP[leftBoolean,rightChar]]"""); true}
//  property("Boolean, Int arguments") = {illTyped("""implicitly[OP[leftBoolean,rightInt]]"""); true}
//  property("Boolean, Long arguments") = {illTyped("""implicitly[OP[leftBoolean,rightLong]]"""); true}
//  property("Boolean, Float arguments") = {illTyped("""implicitly[OP[leftBoolean,rightFloat]]"""); true}
//  property("Boolean, Double arguments") = {illTyped("""implicitly[OP[leftBoolean,rightDouble]]"""); true}
//  property("Boolean, String arguments") = {illTyped("""implicitly[OP[leftBoolean,rightString]]"""); true}
//  property("Boolean, Boolean arguments") = {illTyped("""implicitly[OP[leftBoolean,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//}
