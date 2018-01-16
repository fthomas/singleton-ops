package singleton.ops.impl
import macrocompat.bundle
import scala.reflect.macros.whitebox

/********************************************************************************************************
  * Three arguments type function macro
  *******************************************************************************************************/
@scala.annotation.implicitNotFound("Literal operation has failed.")
trait OpMacro[N, S1, S2, S3] extends Op

@ bundle
object OpMacro {
  type Aux[
  N, 
  S1, 
  S2, 
  S3,
  OutWide0,
  Out0,
  OutNat0 <: shapeless.Nat,
  OutChar0 <: Char with Singleton,
  OutInt0 <: Int with Singleton,
  OutLong0 <: Long with Singleton,
  OutFloat0 <: Float with Singleton,
  OutDouble0 <: Double with Singleton,
  OutString0 <: String with Singleton,
  OutBoolean0 <: Boolean with Singleton,
  OutSymbol0 <: Symbol
  ] = OpMacro[N, S1, S2, S3] {
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
    type OutSymbol = OutSymbol0
  }
  
  implicit def call[
    N,
    S1,
    S2,
    S3,
    OutWide,
    Out,
    OutNat <: shapeless.Nat,
    OutChar <: Char with Singleton,
    OutInt <: Int with Singleton,
    OutLong <: Long with Singleton,
    OutFloat <: Float with Singleton,
    OutDouble <: Double with Singleton,
    OutString <: String with Singleton,
    OutBoolean <: Boolean with Singleton,
    OutSymbol <: Symbol
  ]: Aux[
    N,
    S1,
    S2,
    S3,
    OutWide,
    Out,
    OutNat,
    OutChar,
    OutInt,
    OutLong,
    OutFloat,
    OutDouble,
    OutString,
    OutBoolean,
    OutSymbol
  ] = macro Macro.impl[N, S1, S2, S3]

  final class Macro(val c: whitebox.Context) extends GeneralMacros {
    def impl[
        N : c.WeakTypeTag,
        S1: c.WeakTypeTag,
        S2: c.WeakTypeTag,
        S3: c.WeakTypeTag
    ]: c.Tree =
      materializeOpGen[OpMacro[N, S1, S2, S3]].usingFuncName
  }

//  implicit def valueOfOp[N, S1 : ValueOf, S2 : ValueOf, S3 : ValueOf]
//  (implicit op : OpMacro[N, S1, S2, S3]) : ValueOf[OpMacro[N, S1, S2, S3]] = new ValueOf(op)
}
/*******************************************************************************************************/


/********************************************************************************************************
  * Get function/class argument's type
  *******************************************************************************************************/
object GetArg {
  type Aux[ArgIdx, Out0] = GetArg[ArgIdx]{type Out = Out0}

  @scala.annotation.implicitNotFound("Argument with index ${ArgIdx} not found")
  trait GetArg[ArgIdx] {
    type Out
    val value : Out
  }

  @ bundle
  object GetArg {
    implicit def call[ArgIdx, Out]: Aux[ArgIdx, Out] = macro Macro.impl[ArgIdx]

    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[ArgIdx : c.WeakTypeTag]: c.Tree =
        MaterializeGetArg(c.symbolOf[GetArg[_]], c.symbolOf[Aux[_,_]], c.weakTypeOf[ArgIdx], false)
    }
    implicit def toArgValue[I, Out](i : Aux[I, Out]) : Out = i.value
  }
}
/*******************************************************************************************************/


/********************************************************************************************************
  * Get function/class argument's type
  *******************************************************************************************************/
object GetLHSArg {
  type Aux[ArgIdx, Out0] = GetLHSArg[ArgIdx]{type Out = Out0}

  @scala.annotation.implicitNotFound("Argument with index ${ArgIdx} not found")
  trait GetLHSArg[ArgIdx] {
    type Out
    val value : Out
  }

  @ bundle
  object GetLHSArg {
    implicit def call[ArgIdx, Out]: Aux[ArgIdx, Out] = macro Macro.impl[ArgIdx]

    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[ArgIdx : c.WeakTypeTag]: c.Tree =
        MaterializeGetArg(c.symbolOf[GetLHSArg[_]], c.symbolOf[Aux[_,_]], c.weakTypeOf[ArgIdx], true)
    }
    implicit def toArgValue[I, Out](i : Aux[I, Out]) : Out = i.value
  }
}
/*******************************************************************************************************/
