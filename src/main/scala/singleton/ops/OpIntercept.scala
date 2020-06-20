package singleton.ops
import scala.reflect.macros.whitebox
import impl._

import scala.annotation.implicitNotFound

trait OpIntercept[Op <: HasOut]
object OpIntercept {
  @implicitNotFound("Failed to cache with result ${Out}")
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