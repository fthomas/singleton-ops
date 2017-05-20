package singleton.ops.impl
import macrocompat.bundle
import scala.reflect.macros.whitebox

/********************************************************************************************************
  * Three arguments type function macro
  *******************************************************************************************************/
@scala.annotation.implicitNotFound("Literal operation has failed.")
trait OpMacro[N <: String with Singleton, S1, S2, S3] extends Op

@ bundle
object OpMacro {
  implicit def call[N <: String with Singleton, S1, S2, S3]: OpMacro[N, S1, S2, S3] =
    macro Macro.impl[N, S1, S2, S3]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        N <: String with Singleton: c.WeakTypeTag,
        S1: c.WeakTypeTag,
        S2: c.WeakTypeTag,
        S3: c.WeakTypeTag
    ]: c.Tree =
      materializeOpGen[OpMacro[N, S1, S2, S3], N].usingFuncName
  }

  implicit def valueOfOp[N <: String with Singleton, S1 : ValueOf, S2 : ValueOf, S3 : ValueOf]
  (implicit op : OpMacro[N, S1, S2, S3]) : ValueOf[OpMacro[N, S1, S2, S3]] = new ValueOf(op)
}
/*******************************************************************************************************/
