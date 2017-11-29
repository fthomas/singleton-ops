package singleton.ops

import org.scalacheck.Properties
import singleton.TestUtils._
import shapeless.test.illTyped

class ImplicitConversionSpec extends Properties("ImplicitConversion") {
  property("OpToOp") = wellTyped {val ret : W.`2`.T + W.`1`.T = implicitly[W.`4`.T - W.`1`.T]}
  property("OpToSingleton") = wellTyped {val ret : W.`3`.T = implicitly[W.`4`.T - W.`1`.T]}
//  property("SingletonToOp") = wellTyped {val ret : W.`2`.T + W.`1`.T = 3}
  property("Wrong OpToOp") = {illTyped("""val impl = implicitly[W.`4`.T - W.`2`.T]; val ret : W.`2`.T + W.`1`.T = impl"""); true}
  property("Wrong OpToSingleton") = {illTyped("""val impl = implicitly[W.`4`.T - W.`2`.T]; val ret : W.`3`.T = impl"""); true}
  property("Wrong SingletonToOp") = {illTyped("""val ret : W.`2`.T + W.`1`.T = W.`4`.T"""); true}
//  property("ValueOf[Op]") = wellTyped {val ret : W.`3`.T = valueOf[W.`4`.T - W.`1`.T]}
}