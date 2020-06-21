package singleton.ops
import impl._

import scala.annotation.implicitNotFound
@implicitNotFound("Missing an `OpIntercept` implicit for the operation ${Op}")
trait OpIntercept[Op <: HasOut] extends HasOut
object OpIntercept {
  type Aux[Op <: HasOut, Out0] = OpIntercept[Op]{type Out = Out0}
}