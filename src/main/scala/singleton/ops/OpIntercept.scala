package singleton.ops
import impl._

import scala.annotation.implicitNotFound
@implicitNotFound("Missing an `OpIntercept` implicit for the operation ${Op}")
trait OpIntercept[Op] extends HasOutValue
object OpIntercept {
  type Aux[Op, Out0] = OpIntercept[Op]{type Out = Out0}
}