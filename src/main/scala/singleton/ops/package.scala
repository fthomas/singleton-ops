package singleton
import shapeless.Nat
import singleton.ops.impl._

package object ops {
  /////////////////////////////////////////////////
  //Short of shapeless's Witness
  /////////////////////////////////////////////////
  val W = shapeless.Witness
  /////////////////////////////////////////////////

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
  /////////////////////////////////////////////////
  type SafeNat[P1]          = impl.OpNat[ToNat[Require[IsNat[P1]] ==> P1]]
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

  private val NP = W(0)
  private type NP = NP.T
  private val DefaultRequireMsg = W("Cannot prove requirement Require[...]")
  private type DefaultRequireMsg = DefaultRequireMsg.T
  protected[singleton] val True = W(true)
  protected[singleton] type True = True.T
  protected[singleton] val False = W(false)
  protected[singleton] type False = False.T
  protected[singleton] val SomethingBadHappened = W("Something bad happened")
  protected[singleton] type SomethingBadHappened = SomethingBadHappened.T

  /////////////////////////////////////////////////
  //Special control aliases
  /////////////////////////////////////////////////
  type ITE[Cond,TBody,EBody]= OpMacro[OpId.ITE, Cond, TBody, EBody]
  type ==>[A,B]             = OpMacro[OpId.==>, A, B, NP]
  /////////////////////////////////////////////////

  protected[singleton] type Arg[Num, T, TWide] = OpMacro[OpId.Arg, Num, T, TWide] //Argument for real-time function creation
  protected[singleton] type GetType[Sym] = OpMacro[OpId.GetType, Sym, NP, NP] //Argument for real-time function creation
  type AcceptNonLiteral[P1] = OpMacro[OpId.AcceptNonLiteral, P1, NP, NP]
  type GetArg[ArgIdx]       = OpMacro[OpId.GetArg, ArgIdx, False, NP] //Use to get argument type of class/definition
  type GetLHSArg[ArgIdx]    = OpMacro[OpId.GetArg, ArgIdx, True, NP] //Use to get argument type of the left-hand-side
  type ImplicitFound[Sym]   = OpMacro[OpId.ImplicitFound, GetType[Sym], NP, NP] //Implicit Found boolean indication
  type EnumCount[Sym]       = OpMacro[OpId.EnumCount, GetType[Sym], NP, NP] //Number of direct subclasses
  object GetArg {
    type Aux[ArgIdx, Out] = OpAuxGen[GetArg[ArgIdx], Out]
  }
  object GetLHSArg {
    type Aux[ArgIdx, Out] = OpAuxGen[GetLHSArg[ArgIdx], Out]
  }
  type GetArg0              = GetArg[W.`0`.T]
  object GetArg0 {
    type Aux[Out] = OpAuxGen[GetArg0, Out]
  }
  type GetLHSArg0           = GetLHSArg[W.`0`.T]
  object GetLHSArg0 {
    type Aux[Out] = OpAuxGen[GetLHSArg0, Out]
  }
  type Id[P1]               = OpMacro[OpId.Id, P1, NP, NP]
  type ![P1]                = OpMacro[OpId.!, P1, NP, NP]
  type Require[Cond]        = OpMacro[OpId.Require, Cond, DefaultRequireMsg, NoSym]
  type RequireMsg[Cond,Msg] = OpMacro[OpId.Require, Cond, Msg, NoSym]
  type RequireMsgSym[Cond,Msg,Sym] = OpMacro[OpId.Require, Cond, Msg, GetType[Sym]]
  type Warn                 = impl.Warn
  type ToNat[P1]            = OpMacro[OpId.ToNat, P1, NP, NP]
  type ToChar[P1]           = OpMacro[OpId.ToChar, P1, NP, NP]
  type ToInt[P1]            = OpMacro[OpId.ToInt, P1, NP, NP]
  type ToLong[P1]           = OpMacro[OpId.ToLong, P1, NP, NP]
  type ToFloat[P1]          = OpMacro[OpId.ToFloat, P1, NP, NP]
  type ToDouble[P1]         = OpMacro[OpId.ToDouble, P1, NP, NP]
  type ToString[P1]         = OpMacro[OpId.ToString, P1, NP, NP]
  type IsNat[P1]            = OpMacro[OpId.IsNat, P1, NP, NP]
  type IsChar[P1]           = OpMacro[OpId.IsChar, P1, NP, NP]
  type IsInt[P1]            = OpMacro[OpId.IsInt, P1, NP, NP]
  type IsLong[P1]           = OpMacro[OpId.IsLong, P1, NP, NP]
  type IsFloat[P1]          = OpMacro[OpId.IsFloat, P1, NP, NP]
  type IsDouble[P1]         = OpMacro[OpId.IsDouble, P1, NP, NP]
  type IsString[P1]         = OpMacro[OpId.IsString, P1, NP, NP]
  type IsBoolean[P1]        = OpMacro[OpId.IsBoolean, P1, NP, NP]
  type IsNonLiteral[P1]     = OpMacro[OpId.IsNonLiteral, P1, NP, NP]
  type Reverse[P1]          = OpMacro[OpId.Reverse, P1, NP, NP]
  type Negate[P1]           = OpMacro[OpId.Negate, P1, NP, NP]
  type NumberOfLeadingZeros[P1] = OpMacro[OpId.NumberOfLeadingZeros, P1, NP, NP]

  type +[P1, P2]            = OpMacro[OpId.+, P1, P2, NP]
  type -[P1, P2]            = OpMacro[OpId.-, P1, P2, NP]
  type *[P1, P2]            = OpMacro[OpId.*, P1, P2, NP]
  type /[P1, P2]            = OpMacro[OpId./, P1, P2, NP]
  type %[P1, P2]            = OpMacro[OpId.%, P1, P2, NP]
  type <[P1, P2]            = OpMacro[OpId.<, P1, P2, NP]
  type <=[P1, P2]           = OpMacro[OpId.<=, P1, P2, NP]
  type >=[P1, P2]           = OpMacro[OpId.>=, P1, P2, NP]
  type >[P1, P2]            = OpMacro[OpId.>, P1, P2, NP]
  type ==[P1, P2]           = OpMacro[OpId.==, P1, P2, NP]
  type !=[P1, P2]           = OpMacro[OpId.!=, P1, P2, NP]
  type &&[P1, P2]           = OpMacro[OpId.&&, P1, P2, NP]
  type ||[P1, P2]           = OpMacro[OpId.||, P1, P2, NP]
  type BitwiseAnd[P1, P2]   = OpMacro[OpId.BitwiseAnd, P1, P2, NP]
  type BitwiseOr[P1, P2]    = OpMacro[OpId.BitwiseOr, P1, P2, NP]
  type SubSequence[S, IBeg, IEnd] = OpMacro[OpId.SubSequence, S, IBeg, IEnd]
  type Substring[S, I]       = OpMacro[OpId.Substring, S, I, NP]
  type StartsWith[S, Prefix] = OpMacro[OpId.StartsWith, S, Prefix, NP]
  type EndsWith[S, Suffix]   = OpMacro[OpId.EndsWith, S, Suffix, NP]
  type Head[S]               = OpMacro[OpId.Head, S, NP, NP]
  type Tail[S]               = OpMacro[OpId.Tail, S, NP, NP]
  type Length[S]             = OpMacro[OpId.Length, S, NP, NP]
  type CharAt[S, I]          = OpMacro[OpId.CharAt, S, I, NP]
  type Matches[S, Regex]     = OpMacro[OpId.Matches, S, Regex, NP]
  type FirstMatch[S, Regex]  = OpMacro[OpId.FirstMatch, S, Regex, NP]
  type PrefixMatch[S, Regex] = OpMacro[OpId.PrefixMatch, S, Regex, NP]
  type ReplaceFirstMatch[S, Regex, R] = OpMacro[OpId.ReplaceFirstMatch, S, Regex, R]
  type ReplaceAllMatches[S, Regex, R] = OpMacro[OpId.ReplaceAllMatches, S, Regex, R]

  type CompileTime[C]       = Require[ITE[IsNonLiteral[C], W.`true`.T, C]]
  type RunTime[R]           = SafeBoolean[IsNonLiteral[R]]

  object math {
    type Pi                 = W.`3.141592653589793`.T
    type E                  = W.`2.718281828459045`.T
    type Abs[P1]            = OpMacro[OpId.Abs, P1, NP, NP]
    type Min[P1, P2]        = OpMacro[OpId.Min, P1, P2, NP]
    type Max[P1, P2]        = OpMacro[OpId.Max, P1, P2, NP]
    type Pow[P1, P2]        = OpMacro[OpId.Pow, P1, P2, NP]
    type Floor[P1]          = OpMacro[OpId.Floor, P1, NP, NP]
    type Ceil[P1]           = OpMacro[OpId.Ceil, P1, NP, NP]
    type Round[P1]          = OpMacro[OpId.Round, P1, NP, NP]
    type Sin[P1]            = OpMacro[OpId.Sin, P1, NP, NP]
    type Cos[P1]            = OpMacro[OpId.Cos, P1, NP, NP]
    type Tan[P1]            = OpMacro[OpId.Tan, P1, NP, NP]
    type Sqrt[P1]           = OpMacro[OpId.Sqrt, P1, NP, NP]
    type Log[P1]            = OpMacro[OpId.Log, P1, NP, NP]
    type Log10[P1]          = OpMacro[OpId.Log10, P1, NP, NP]
  }


  /////////////////////////////////////////////////
  //Implicit proof that
  //singleton type is equivalent to type operation
  //E.g. val four : 2 + 2 = 4
  /////////////////////////////////////////////////
  implicit def singletonToOp[
  X <: Singleton, N, S1, S2, S3, OP_OUT
  ](x: X)
   (implicit
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
  N, S1, S2, S3, OP_OUT
  ](op : OpMacro[N, S1, S2, S3])
   (implicit
    opaux: OpAuxGen[OpMacro[N, S1, S2, S3], OP_OUT],
    id : Id[OP_OUT]
   ) : OP_OUT = id.value.asInstanceOf[OP_OUT]
  /////////////////////////////////////////////////


  /////////////////////////////////////////////////
  //Implicit proof that
  //type operation is equivalent to a different type operation
  //E.g. val four : 1 + 3 = implicitly[2 + 2]
  /////////////////////////////////////////////////
  implicit def opToOp[
  NA, SA1, SA2, SA3,
  NB, SB1, SB2, SB3,
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
