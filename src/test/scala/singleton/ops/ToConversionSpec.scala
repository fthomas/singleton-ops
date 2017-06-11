package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class ToConversionSpec extends Properties("ToConversion") {
  //ToNat relies on Int and already checked within IdSpec

  property("Nat to Char") = verifyOp1Args[ToChar,shapeless.Nat._1,'\u0001']
  property("Char to Char") = verifyOp1Args[ToChar,'\u0002','\u0002']
  property("Int to Char") = verifyOp1Args[ToChar,3,'\u0003']
  property("Long to Char") = verifyOp1Args[ToChar,4L,'\u0004']
  property("Float to Char") = verifyOp1Args[ToChar,5.0f,'\u0005']
  property("Double to Char") = verifyOp1Args[ToChar,6.0,'\u0006']
  property("String to Char") = {illTyped("""implicitly[ToChar["7"]]"""); true}
  property("Boolean to Char") = {illTyped("""implicitly[ToChar[true]]"""); true}

  property("Nat to Int") = verifyOp1Args[ToInt,shapeless.Nat._1,1]
  property("Char to Int") = verifyOp1Args[ToInt,'\u0002',2]
  property("Int to Int") = verifyOp1Args[ToInt,3,3]
  property("Long to Int") = verifyOp1Args[ToInt,4L,4]
  property("Float to Int") = verifyOp1Args[ToInt,5.0f,5]
  property("Double to Int") = verifyOp1Args[ToInt,6.0,6]
  property("String to Int") = verifyOp1Args[ToInt,"7",7]
  property("Boolean to Int") = {illTyped("""implicitly[ToInt[true]]"""); true}

  property("Nat to Long") = verifyOp1Args[ToLong,shapeless.Nat._1,1L]
  property("Char to Long") = verifyOp1Args[ToLong,'\u0002',2L]
  property("Int to Long") = verifyOp1Args[ToLong,3,3L]
  property("Long to Long") = verifyOp1Args[ToLong,4L,4L]
  property("Float to Long") = verifyOp1Args[ToLong,5.0f,5L]
  property("Double to Long") = verifyOp1Args[ToLong,6.0,6L]
  property("String to Long") = verifyOp1Args[ToLong,"7",7L]
  property("Boolean to Long") = {illTyped("""implicitly[ToLong[true]]"""); true}

  property("Nat to Float") = verifyOp1Args[ToFloat,shapeless.Nat._1,1.0f]
  property("Char to Float") = verifyOp1Args[ToFloat,'\u0002',2.0f]
  property("Int to Float") = verifyOp1Args[ToFloat,3,3.0f]
  property("Long to Float") = verifyOp1Args[ToFloat,4L,4.0f]
  property("Float to Float") = verifyOp1Args[ToFloat,5.0f,5.0f]
  property("Double to Float") = verifyOp1Args[ToFloat,6.0,6.0f]
  property("String to Float") = verifyOp1Args[ToFloat,"7",7.0f]
  property("Boolean to Float") = {illTyped("""implicitly[ToFloat[true]]"""); true}

  property("Nat to Double") = verifyOp1Args[ToDouble,shapeless.Nat._1,1.0]
  property("Char to Double") = verifyOp1Args[ToDouble,'\u0002',2.0]
  property("Int to Double") = verifyOp1Args[ToDouble,3,3.0]
  property("Long to Double") = verifyOp1Args[ToDouble,4L,4.0]
  property("Float to Double") = verifyOp1Args[ToDouble,5.0f,5.0]
  property("Double to Double") = verifyOp1Args[ToDouble,6.0,6.0]
  property("String to Double") = verifyOp1Args[ToDouble,"7",7.0]
  property("Boolean to Double") = {illTyped("""implicitly[ToDouble[true]]"""); true}

  property("Nat to String") = verifyOp1Args[ToString,shapeless.Nat._1,"1"]
  property("Char to String") = verifyOp1Args[ToString,'2',"2"]
  property("Int to String") = verifyOp1Args[ToString,3,"3"]
  property("Long to String") = verifyOp1Args[ToString,4L,"4"]
  property("Float to String") = verifyOp1Args[ToString,5.0f,"5.0"]
  property("Double to String") = verifyOp1Args[ToString,6.0,"6.0"]
  property("String to String") = verifyOp1Args[ToString,"7","7"]
  property("Boolean to String") = verifyOp1Args[ToString,true,"true"]
}