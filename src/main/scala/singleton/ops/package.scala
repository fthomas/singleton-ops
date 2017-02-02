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
  //Short aliases for casting operation type value
  //Used as upper operation only.
  //E.g. SafeInt[Something[Nice] + 1] OK
  //     Something[SafeInt[Nice] + 1] BAD
  /////////////////////////////////////////////////
  type SafeNat[P1]          = impl.OpNat[Require[IsNat[P1]] ==> P1]
  type SafeChar[P1]         = impl.OpChar[Require[IsChar[P1]] ==> P1]
  type SafeInt[P1]          = impl.OpInt[Require[IsInt[P1]] ==> P1]
  type SafeLong[P1]         = impl.OpLong[Require[IsLong[P1]] ==> P1]
  type SafeFloat[P1]        = impl.OpFloat[Require[IsFloat[P1]] ==> P1]
  type SafeDouble[P1]       = impl.OpDouble[Require[IsDouble[P1]] ==> P1]
  type SafeString[P1]       = impl.OpString[Require[IsString[P1]] ==> P1]
  type SafeBoolean[P1]      = impl.OpBoolean[Require[IsBoolean[P1]] ==> P1]
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
  type IsBoolean[P1]        = OpMacro["IsBoolean",P1, 0, 0]
  type Reverse[P1]          = OpMacro["Reverse",P1, 0, 0]
  type Negate[P1]           = OpMacro["Negate",P1, 0, 0]
  type Abs[P1]              = OpMacro["Abs",P1, 0, 0]
  type Print[P1]            = OpMacro["Print",P1, 0, 0]
  type Floor[P1]            = OpMacro["Floor",P1, 0, 0]
  type Ceil[P1]             = OpMacro["Ceil",P1, 0, 0]
  type Round[P1]            = OpMacro["Round",P1, 0, 0]
  type Sin[P1]              = OpMacro["Sin",P1, 0, 0]
  type Cos[P1]              = OpMacro["Cos",P1, 0, 0]
  type Tan[P1]              = OpMacro["Tan",P1, 0, 0]
  type Sqrt[P1]             = OpMacro["Sqrt",P1, 0, 0]

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
  type Pow[P1, P2]          = OpMacro["Pow",P1, P2, 0]
  type Min[P1, P2]          = OpMacro["Min",P1, P2, 0]
  type Max[P1, P2]          = OpMacro["Max",P1, P2, 0]
  type Substring[P1, P2]    = OpMacro["Substring",P1, P2, 0]
  type Length[P1]           = OpMacro["Length", P1, 0, 0]
  type CharAt[P1, P2]       = OpMacro["CharAt", P1, P2, 0]



  /////////////////////////////////////////////////
  //Implicit proof that
  //singleton type is equivalent to type operation
  //E.g. val four : 2 + 2 = 4
  /////////////////////////////////////////////////
  implicit def singletonToOp[
  X <: Singleton, N <: XString, S1, S2, S3, OP_OUT
  ](x: X)
   (implicit
    v : ValueOf[X],
    op : OpMacro[N, S1, S2, S3],
    opaux: OpAuxGen[OpMacro[N, S1, S2, S3], OP_OUT],
    check : Require[X == OP_OUT]
   ) : OpMacro[N, S1, S2, S3] = op
  /////////////////////////////////////////////////


  /////////////////////////////////////////////////
  //Implicit proof that
  //type operation is equivalent to singleton type
  //E.g. val four : 4 = implicitly[2 + 2]
  /////////////////////////////////////////////////
  implicit def opToSingleton[
  N <: XString, S1, S2, S3, OP_OUT
  ](op : OpMacro[N, S1, S2, S3])
   (implicit
    opaux: OpAuxGen[OpMacro[N, S1, S2, S3], OP_OUT],
    v : ValueOf[OP_OUT]
   ) : OP_OUT = valueOf[OP_OUT]
  /////////////////////////////////////////////////


  /////////////////////////////////////////////////
  //Implicit proof that
  //type operation is equivalent to a different type operation
  //E.g. val four : 1 + 3 = implicitly[2 + 2]
  /////////////////////////////////////////////////
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
  /////////////////////////////////////////////////

}
