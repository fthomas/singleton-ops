package singleton
import shapeless.Nat
import singleton.ops.impl._

package object ops {
  /////////////////////////////////////////////////
  //Short aliases of singleton types
  /////////////////////////////////////////////////
  type XChar                = Char with Singleton
  type XInt                 = Int with Singleton
  type XLong                = Long with Singleton
  type XFloat               = Float with Singleton
  type XDouble              = Double with Singleton
  type XString              = String with Singleton
  type XBoolean             = Boolean with Singleton
  /////////////////////////////////////////////////

  /////////////////////////////////////////////////
  //Short aliases of auxiliary operation types
  /////////////////////////////////////////////////
  type OpAuxGen[O <: Op,      Ret_Out]              = OpGen.Aux[O, Ret_Out]
  type OpAuxNat[O <: Op,      Ret_Out <: Nat]       = OpNat.Aux[O, Ret_Out]
  type OpAuxChar[O <: Op,     Ret_Out <: XChar]     = OpChar.Aux[O, Ret_Out]
  type OpAuxInt[O <: Op,      Ret_Out <: XInt]      = OpInt.Aux[O, Ret_Out]
  type OpAuxLong[O <: Op,     Ret_Out <: XLong]     = OpLong.Aux[O, Ret_Out]
  type OpAuxFloat[O <: Op,    Ret_Out <: XFloat]    = OpFloat.Aux[O, Ret_Out]
  type OpAuxDouble[O <: Op,   Ret_Out <: XDouble]   = OpDouble.Aux[O, Ret_Out]
  type OpAuxString[O <: Op,   Ret_Out <: XString]   = OpString.Aux[O, Ret_Out]
  type OpAuxBoolean[O <: Op,  Ret_Out <: XBoolean]  = OpBoolean.Aux[O, Ret_Out]
  /////////////////////////////////////////////////

  /////////////////////////////////////////////////
  //Special control aliases
  /////////////////////////////////////////////////
  type ITE[Cond,TBody,EBody]= OpMacro["ITE",Cond, TBody, EBody]
  type While[Cond,Body,Ret] = OpMacro["While",Cond, Body, Ret]
  type ==>[A,B]             = OpMacro["==>",A, B, 0]
  type :=[Name,Value]       = SV[Name, Value]
  type +=[Name,Value]       = SV[Name, GV[Name] + Value]
  type -=[Name,Value]       = SV[Name, GV[Name] - Value]
  type *=[Name,Value]       = SV[Name, GV[Name] * Value]
  type /=[Name,Value]       = SV[Name, GV[Name] / Value]
  type SV[Name,Value]       = OpMacro["SV",Name, Value, 0]
  type GV[Name]             = OpMacro["GV",Name, 0, 0]
  /////////////////////////////////////////////////

  type ![P1]                = OpMacro["!",P1, 0, 0]
  type Require[P1]          = OpMacro["Require",P1, 0, 0]
  type ToNat[P1]            = OpMacro["ToNat",P1, 0, 0]
  type ToChar[P1]           = OpMacro["ToChar",P1, 0, 0]
  type ToInt[P1]            = OpMacro["ToInt",P1, 0, 0]
  type ToLong[P1]           = OpMacro["ToLong",P1, 0, 0]
  type ToFloat[P1]          = OpMacro["ToFloat",P1, 0, 0]
  type ToDouble[P1]         = OpMacro["ToDouble",P1, 0, 0]
  type ToString[P1]         = OpMacro["ToString",P1, 0, 0]
  type IsNat[P1]            = OpMacro["IsNat",P1, 0, 0]
  type IsChar[P1]           = OpMacro["IsChar",P1, 0, 0]
  type IsInt[P1]            = OpMacro["IsInt",P1, 0, 0]
  type IsLong[P1]           = OpMacro["IsLong",P1, 0, 0]
  type IsFloat[P1]          = OpMacro["IsFloat",P1, 0, 0]
  type IsDouble[P1]         = OpMacro["IsDouble",P1, 0, 0]
  type IsString[P1]         = OpMacro["IsString",P1, 0, 0]
  type Reverse[P1]          = OpMacro["Reverse",P1, 0, 0]
  type Negate[P1]           = OpMacro["Negate",P1, 0, 0]
  type Abs[P1]              = OpMacro["Abs",P1, 0, 0]

  type +[P1, P2]            = OpMacro["+",P1, P2, 0]
  type -[P1, P2]            = OpMacro["-",P1, P2, 0]
  type *[P1, P2]            = OpMacro["*",P1, P2, 0]
  type /[P1, P2]            = OpMacro["/",P1, P2, 0]
  type %[P1, P2]            = OpMacro["%",P1, P2, 0]
  type <[P1, P2]            = OpMacro["<",P1, P2, 0]
  type <=[P1, P2]           = OpMacro["<=",P1, P2, 0]
  type >=[P1, P2]           = OpMacro[">=",P1, P2, 0]
  type >[P1, P2]            = OpMacro[">",P1, P2, 0]
  type ==[P1, P2]           = OpMacro["==",P1, P2, 0]
  type !=[P1, P2]           = OpMacro["!=",P1, P2, 0]
  type &&[P1, P2]           = OpMacro["&&",P1, P2, 0]
  type ||[P1, P2]           = OpMacro["||",P1, P2, 0]
  type Min[P1, P2]          = OpMacro["Min",P1, P2, 0]
  type Max[P1, P2]          = OpMacro["Max",P1, P2, 0]
  type Substring[P1, P2]    = OpMacro["Substring",P1, P2, 0]
  type Length[P1]           = OpMacro["Length", P1, 0, 0]
  type CharAt[P1, P2]       = OpMacro["CharAt", P1, P2, 0]



  implicit def singletonToOp[
  X <: Singleton, N <: XString, S1, S2, S3, OP_OUT
  ](x: X)
   (implicit
    v : ValueOf[X],
    op : OpMacro[N, S1, S2, S3],
    opaux: OpAuxGen[OpMacro[N, S1, S2, S3], OP_OUT],
    check : Require[X == OP_OUT]
   ) : OpMacro[N, S1, S2, S3] = op


  implicit def opToSingleton[
  N <: XString, S1, S2, S3, OP_OUT
  ](op : OpMacro[N, S1, S2, S3])
   (implicit
    opaux: OpAuxGen[OpMacro[N, S1, S2, S3], OP_OUT],
    v : ValueOf[OP_OUT]
   ) : OP_OUT = valueOf[OP_OUT]


  implicit def opToOp[
  NA <: XString, SA1, SA2, SA3,
  NB <: XString, SB1, SB2, SB3,
  OP_OUTA,
  OP_OUTB
  ](opA : OpMacro[NA, SA1, SA2, SA3])
   (implicit
    opauxA: OpAuxGen[OpMacro[NA, SA1, SA2, SA3], OP_OUTA],
    opauxB: OpAuxGen[OpMacro[NB, SB1, SB2, SB3], OP_OUTB],
    opB : OpMacro[NB, SB1, SB2, SB3],
    check : Require[OP_OUTA == OP_OUTB]
  ) : OpMacro[NB, SB1, SB2, SB3] = opB


  //  implicit def convNat[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]{type OutWide = Nat}) : Nat = op.valueWide
//  implicit def convChar[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]{type OutWide = Char}) : Char = op.valueWide
//  implicit def convInt[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]) : Int = op.valueWide.asInstanceOf
//  implicit def convLong[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]{type OutWide = Long}) : Long = op.valueWide
//  implicit def convFloat[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]{type OutWide = Float}) : Float = op.valueWide
//  implicit def convDouble[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]{type OutWide = Double}) : Double = op.valueWide
//  implicit def convString[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]{type OutWide = String}) : String = op.valueWide
//  implicit def convBoolean[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]{type OutWide = Boolean}) : Boolean = op.valueWide

//  def valueOf2[T](implicit vt: ValueOf[T]): T {} = vt.value
//  def valueOf2[OP <: OpMacro[_ <: String with Singleton, _, _, _]]
//  (implicit op: OP) : op.Out = op.value

}
