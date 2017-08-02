//package singleton.ops
//
//import org.scalacheck.{Prop, Properties}
//import shapeless.test.illTyped
//import singleton.TestUtils._
//
//class STSpec extends Properties("<") {
//  type OP[L,R] = <[L,R]
//  type leftNat = shapeless.Nat._1
//  type leftChar = '\u0001'
//  type leftInt = 1
//  type leftLong = 1L
//  type leftFloat = 1.0f
//  type leftDouble = 1.0
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
//  def verifySTNum[L,R](implicit
//                       verifyFalse: Verify[L < Negate[R], False],
//                       verifyFalse2: Verify[L < L, False],
//                       verifyTrue: Verify[L < R, True]) : Prop = wellTyped {}
//
//  ////////////////////////////////////////////////////////////////////////
//  // Nat op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Nat, Nat arguments") = verifySTNum[leftNat,rightNat]
//  property("Nat, Char arguments") = verifySTNum[leftNat,rightChar]
//  property("Nat, Int arguments") = verifySTNum[leftNat,rightInt]
//  property("Nat, Long arguments") = verifySTNum[leftNat,rightLong]
//  property("Nat, Float arguments") = verifySTNum[leftNat,rightFloat]
//  property("Nat, Double arguments") = verifySTNum[leftNat,rightDouble]
//  property("Nat, String arguments") = {illTyped("""implicitly[OP[leftNat,rightString]]"""); true}
//  property("Nat, Boolean arguments") = {illTyped("""implicitly[OP[leftNat,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Char op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Char, Nat arguments") = verifySTNum[leftChar,rightNat]
//  property("Char, Char arguments") = verifySTNum[leftChar,rightChar]
//  property("Char, Int arguments") = verifySTNum[leftChar,rightInt]
//  property("Char, Long arguments") = verifySTNum[leftChar,rightLong]
//  property("Char, Float arguments") = verifySTNum[leftChar,rightFloat]
//  property("Char, Double arguments") = verifySTNum[leftChar,rightDouble]
//  property("Char, String arguments") = {illTyped("""implicitly[OP[leftChar,rightString]]"""); true}
//  property("Char, Boolean arguments") = {illTyped("""implicitly[OP[leftChar,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Int op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Int, Nat arguments") = verifySTNum[leftInt,rightNat]
//  property("Int, Char arguments") = verifySTNum[leftInt,rightChar]
//  property("Int, Int arguments") = verifySTNum[leftInt,rightInt]
//  property("Int, Long arguments") = verifySTNum[leftInt,rightLong]
//  property("Int, Float arguments") = verifySTNum[leftInt,rightFloat]
//  property("Int, Double arguments") = verifySTNum[leftInt,rightDouble]
//  property("Int, String arguments") = {illTyped("""implicitly[OP[leftInt,rightString]]"""); true}
//  property("Int, Boolean arguments") = {illTyped("""implicitly[OP[leftInt,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Long op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Long, Nat arguments") = verifySTNum[leftLong,rightNat]
//  property("Long, Char arguments") = verifySTNum[leftLong,rightChar]
//  property("Long, Int arguments") = verifySTNum[leftLong,rightInt]
//  property("Long, Long arguments") = verifySTNum[leftLong,rightLong]
//  property("Long, Float arguments") = verifySTNum[leftLong,rightFloat]
//  property("Long, Double arguments") = verifySTNum[leftLong,rightDouble]
//  property("Long, String arguments") = {illTyped("""implicitly[OP[leftLong,rightString]]"""); true}
//  property("Long, Boolean arguments") = {illTyped("""implicitly[OP[leftLong,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Float op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Float, Nat arguments") = verifySTNum[leftFloat,rightNat]
//  property("Float, Char arguments") = verifySTNum[leftFloat,rightChar]
//  property("Float, Int arguments") = verifySTNum[leftFloat,rightInt]
//  property("Float, Long arguments") = verifySTNum[leftFloat,rightLong]
//  property("Float, Float arguments") = verifySTNum[leftFloat,rightFloat]
//  property("Float, Double arguments") = verifySTNum[leftFloat,rightDouble]
//  property("Float, String arguments") = {illTyped("""implicitly[OP[leftFloat,rightString]]"""); true}
//  property("Float, Boolean arguments") = {illTyped("""implicitly[OP[leftFloat,rightBoolean]]"""); true}
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Double op XXX
//  ////////////////////////////////////////////////////////////////////////
//  property("Double, Nat arguments") = verifySTNum[leftDouble,rightNat]
//  property("Double, Char arguments") = verifySTNum[leftDouble,rightChar]
//  property("Double, Int arguments") = verifySTNum[leftDouble,rightInt]
//  property("Double, Long arguments") = verifySTNum[leftDouble,rightLong]
//  property("Double, Float arguments") = verifySTNum[leftDouble,rightFloat]
//  property("Double, Double arguments") = verifySTNum[leftDouble,rightDouble]
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
