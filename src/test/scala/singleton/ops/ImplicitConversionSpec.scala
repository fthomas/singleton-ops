package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._
import shapeless.test.illTyped

class ImplicitConversionSpec extends Properties("ImplicitConversion") {
  property("OpToOp") = wellTyped {val ret : 2 + 1 = implicitly[4 - 1]}
  property("OpToSingleton") = wellTyped {val ret : 3 = implicitly[4 - 1]}
  property("SingletonToOp") = wellTyped {val ret : 2 + 1 = 3}
  property("Wrong OpToOp") = {illTyped("""val ret : 2 + 1 = implicitly[4 - 2]"""); true}
  property("Wrong OpToSingleton") = {illTyped("""val ret : 3 = implicitly[4 - 2]"""); true}
  property("Wrong SingletonToOp") = {illTyped("""val ret : 2 + 1 = 4"""); true}
//  property("ValueOf[Op]") = wellTyped {val ret : 3 = valueOf[4 - 1]}
}