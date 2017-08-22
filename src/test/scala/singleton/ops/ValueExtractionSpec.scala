//package singleton.ops
//
//import org.scalacheck.Properties
//
//class ValueExtractionNatSpec extends Properties("ValueExtractionNatSpec") {
//  type Fin = shapeless.Nat._1
//  final val fin = shapeless.Nat._1
//  property("Extracting from Safe Nat Number") = {
//    val ret = shapeless.the[ToNat[Id[fin.type]]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Nat Op Wrapper") = {
//    val a = shapeless.the[SafeNat[Fin]]
//    implicitly[a.Out =:= Fin]
//    a.value == fin
//  }
//}
//
//class ValueExtractionCharSpec extends Properties("ValueExtractionCharSpec") {
//  type Fin = '\u0003'
//  final val fin = '\u0003'
//  property("Extracting from Safe Char Number") = {
//    val ret = shapeless.the[Id[fin.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Char Number") = {
//    val a = fin
//    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    implicitly[ret.Out =:= Char]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Char Op") = {
//    val a = shapeless.the[Id[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Char Op") = {
//    val a = fin
//    val b = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    val ret = shapeless.the[AcceptNonLiteral[Id[b.type]]]
//    implicitly[ret.Out =:= Char]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Char Op Wrapper") = {
//    val a = shapeless.the[SafeChar[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//}
//
//class ValueExtractionIntSpec extends Properties("ValueExtractionIntSpec") {
//  type Fin = 3
//  final val fin = 3
//  property("Extracting from Safe Int Number") = {
//    val ret = shapeless.the[Id[fin.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Int Number") = {
//    val a = fin
//    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    implicitly[ret.Out =:= Int]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Int Op") = {
//    val a = shapeless.the[Id[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Int Op") = {
//    val a = fin
//    val b = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    val ret = shapeless.the[AcceptNonLiteral[Id[b.type]]]
//    implicitly[ret.Out =:= Int]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Int Op Wrapper") = {
//    val a = shapeless.the[SafeInt[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//}
//
//class ValueExtractionLongSpec extends Properties("ValueExtractionLongSpec") {
//  type Fin = 3L
//  final val fin = 3L
//  property("Extracting from Safe Long Number") = {
//    val ret = shapeless.the[Id[fin.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Long Number") = {
//    val a = fin
//    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    implicitly[ret.Out =:= Long]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Long Op") = {
//    val a = shapeless.the[Id[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Long Op") = {
//    val a = fin
//    val b = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    val ret = shapeless.the[AcceptNonLiteral[Id[b.type]]]
//    implicitly[ret.Out =:= Long]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Long Op Wrapper") = {
//    val a = shapeless.the[SafeLong[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//}
//
//class ValueExtractionFloatSpec extends Properties("ValueExtractionFloatSpec") {
//  type Fin = 3.0f
//  final val fin = 3.0f
//  property("Extracting from Safe Float Number") = {
//    val ret = shapeless.the[Id[fin.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Float Number") = {
//    val a = fin
//    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    implicitly[ret.Out =:= Float]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Float Op") = {
//    val a = shapeless.the[Id[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Float Op") = {
//    val a = fin
//    val b = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    val ret = shapeless.the[AcceptNonLiteral[Id[b.type]]]
//    implicitly[ret.Out =:= Float]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Float Op Wrapper") = {
//    val a = shapeless.the[SafeFloat[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//}
//
//class ValueExtractionDoubleSpec extends Properties("ValueExtractionDoubleSpec") {
//  type Fin = 3.0
//  final val fin = 3.0
//  property("Extracting from Safe Double Number") = {
//    val ret = shapeless.the[Id[fin.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Double Number") = {
//    val a = fin
//    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    implicitly[ret.Out =:= Double]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Double Op") = {
//    val a = shapeless.the[Id[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Double Op") = {
//    val a = fin
//    val b = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    val ret = shapeless.the[AcceptNonLiteral[Id[b.type]]]
//    implicitly[ret.Out =:= Double]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Double Op Wrapper") = {
//    val a = shapeless.the[SafeDouble[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//}
//
//class ValueExtractionStringSpec extends Properties("ValueExtractionStringSpec") {
//  type Fin = "3"
//  final val fin = "3"
//  property("Extracting from Safe String Number") = {
//    val ret = shapeless.the[Id[fin.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe String Number") = {
//    val a = fin
//    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    implicitly[ret.Out =:= String]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe String Op") = {
//    val a = shapeless.the[Id[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe String Op") = {
//    val a = fin
//    val b = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    val ret = shapeless.the[AcceptNonLiteral[Id[b.type]]]
//    implicitly[ret.Out =:= String]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe String Op Wrapper") = {
//    val a = shapeless.the[SafeString[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//}
//
//class ValueExtractionBooleanSpec extends Properties("ValueExtractionBooleanSpec") {
//  type Fin = true
//  final val fin = true
//  property("Extracting from Safe Boolean Number") = {
//    val ret = shapeless.the[Id[fin.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Boolean Number") = {
//    val a = fin
//    val ret = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    implicitly[ret.Out =:= Boolean]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Boolean Op") = {
//    val a = shapeless.the[Id[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//
//  property("Extracting from Unsafe Boolean Op") = {
//    val a = fin
//    val b = shapeless.the[AcceptNonLiteral[Id[a.type]]]
//    val ret = shapeless.the[AcceptNonLiteral[Id[b.type]]]
//    implicitly[ret.Out =:= Boolean]
//    ret.value == fin
//  }
//
//  property("Extracting from Safe Boolean Op Wrapper") = {
//    val a = shapeless.the[SafeBoolean[Fin]]
//    val ret = shapeless.the[Id[a.type]]
//    implicitly[ret.Out =:= Fin]
//    ret.value == fin
//  }
//}
//
