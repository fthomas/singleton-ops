package singleton.ops.impl
import scala.reflect.macros.whitebox

/********************************************************************************************************
  * Three arguments type function macro
  *******************************************************************************************************/
@scala.annotation.implicitNotFound("Literal operation has failed.")
trait OpMacro[SymId, Args] extends Op

object OpMacro {
  type Aux[
  SymId, 
  Args,
  OutWide0,
  Out0,
  OutNat0 <: shapeless.Nat,
  OutChar0 <: Char with Singleton,
  OutInt0 <: Int with Singleton,
  OutLong0 <: Long with Singleton,
  OutFloat0 <: Float with Singleton,
  OutDouble0 <: Double with Singleton,
  OutString0 <: String with Singleton,
  OutBoolean0 <: Boolean with Singleton
  ] = OpMacro[SymId, Args] {
    type OutWide = OutWide0
    type Out = Out0
    type OutNat = OutNat0
    type OutChar = OutChar0
    type OutInt = OutInt0
    type OutLong = OutLong0
    type OutFloat = OutFloat0
    type OutDouble = OutDouble0
    type OutString = OutString0
    type OutBoolean = OutBoolean0
  }
  
  implicit def call[
    SymId,
    Args,
    OutWide,
    Out,
    OutNat <: shapeless.Nat,
    OutChar <: Char with Singleton,
    OutInt <: Int with Singleton,
    OutLong <: Long with Singleton,
    OutFloat <: Float with Singleton,
    OutDouble <: Double with Singleton,
    OutString <: String with Singleton,
    OutBoolean <: Boolean with Singleton
  ]: Aux[
    SymId,
    Args,
    OutWide,
    Out,
    OutNat,
    OutChar,
    OutInt,
    OutLong,
    OutFloat,
    OutDouble,
    OutString,
    OutBoolean
  ] = macro Macro.impl[SymId, Args]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        SymId : c.WeakTypeTag,
        Args  : c.WeakTypeTag
    ]: c.Tree =
      materializeOpGen[OpMacro[SymId, Args]].usingFuncName
  }

//  implicit def valueOfOp[N, S1 : ValueOf, S2 : ValueOf, S3 : ValueOf]
//  (implicit op : OpMacro[N, S1, S2, S3]) : ValueOf[OpMacro[N, S1, S2, S3]] = new ValueOf(op)
}
/*******************************************************************************************************/
