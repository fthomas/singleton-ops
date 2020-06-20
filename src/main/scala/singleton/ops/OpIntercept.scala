package singleton.ops
import scala.reflect.macros.whitebox
import impl._

import scala.annotation.implicitNotFound

trait OpIntercept[Op <: HasOut] extends HasOut
object OpIntercept {
  type Aux[Op <: HasOut, Out0] = OpIntercept[Op]{type Out = Out0}
  @implicitNotFound("Failed to cache op ${Op} with result ${Out}")
  trait CacheResult[Op <: HasOut, Out]
  object CacheResult {
    implicit def call[Op <: HasOut, Out] : CacheResult[Op, Out] = macro Macro.materializeCacheResult[Op, Out]
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def materializeCacheResult[
        Op : c.WeakTypeTag,
        Out: c.WeakTypeTag,
      ]: c.Tree = cacheOpInterceptResult[Op, Out]
    }

  }
}