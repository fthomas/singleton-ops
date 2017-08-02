package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._
import shapeless.test.illTyped

class ImplicitConversionSpec extends Properties("ImplicitConversion") {
  property("OpToOp") = wellTyped {val ret : W.`2`.T + W.`1`.T = implicitly[W.`4`.T - W.`1`.T]}
  property("OpToSingleton") = wellTyped {val ret : W.`3`.T = implicitly[W.`4`.T - W.`1`.T]}
  property("SingletonToOp") = wellTyped {val ret : W.`2`.T + W.`1`.T = W(3).value}
  property("Wrong OpToOp") = {illTyped("""val ret : W.`2`.T + W.`1`.T = implicitly[W.`4`.T - W.`2`.T]"""); true}
  property("Wrong OpToSingleton") = {illTyped("""val ret : W.`3`.T = implicitly[W.`4`.T - W.`2`.T]"""); true}
  property("Wrong SingletonToOp") = {illTyped("""val ret : W.`2`.T + W.`1`.T = W.`4`.T"""); true}
//  property("ValueOf[Op]") = wellTyped {val ret : W.`3`.T = valueOf[W.`4`.T - W.`1`.T]}
}