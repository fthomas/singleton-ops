package singleton.ops
import scala.reflect.macros.whitebox
import impl._

import scala.annotation.implicitNotFound

trait OpIntercept[Op <: HasOut] extends HasOut
object OpIntercept {
  type Aux[Op <: HasOut, Out0] = OpIntercept[Op]{type Out = Out0}
  @implicitNotFound("Failed to cache op ${Op} with result ${Out}")
  trait CacheResult[Out]
  object CacheResult {
    implicit def call[Out] : CacheResult[Out] = macro Macro.materializeCacheResult[Out]
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def materializeCacheResult[
        Out: c.WeakTypeTag
      ]: c.Tree = cacheOpInterceptResult[Out]
    }

  }
}