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
  type OpGenAux[O,            Ret_Out]              = OpGen.Aux[O, Ret_Out]
  type OpNatAux[O <: Op,      Ret_Out <: Nat]       = OpNat.Aux[O, Ret_Out]
  type OpCharAux[O <: Op,     Ret_Out <: XChar]     = OpChar.Aux[O, Ret_Out]
  type OpIntAux[O <: Op,      Ret_Out <: XInt]      = OpInt.Aux[O, Ret_Out]
  type OpLongAux[O <: Op,     Ret_Out <: XLong]     = OpLong.Aux[O, Ret_Out]
  type OpFloatAux[O <: Op,    Ret_Out <: XFloat]    = OpFloat.Aux[O, Ret_Out]
  type OpDoubleAux[O <: Op,   Ret_Out <: XDouble]   = OpDouble.Aux[O, Ret_Out]
  type OpStringAux[O <: Op,   Ret_Out <: XString]   = OpString.Aux[O, Ret_Out]
  type OpBooleanAux[O <: Op,  Ret_Out <: XBoolean]  = OpBoolean.Aux[O, Ret_Out]
  type OpInterceptAux[F, Out]                       = OpIntercept.Aux[F, Out]

  @deprecated("Please use `OpGenAux` instead")
  type OpAuxGen[O,            Ret_Out]              = OpGen.Aux[O, Ret_Out]
  @deprecated("Please use `OpNatAux` instead")
  type OpAuxNat[O <: Op,      Ret_Out <: Nat]       = OpNat.Aux[O, Ret_Out]
  @deprecated("Please use `OpCharAux` instead")
  type OpAuxChar[O <: Op,     Ret_Out <: XChar]     = OpChar.Aux[O, Ret_Out]
  @deprecated("Please use `OpIntAux` instead")
  type OpAuxInt[O <: Op,      Ret_Out <: XInt]      = OpInt.Aux[O, Ret_Out]
  @deprecated("Please use `OpLongAux` instead")
  type OpAuxLong[O <: Op,     Ret_Out <: XLong]     = OpLong.Aux[O, Ret_Out]
  @deprecated("Please use `OpFloatAux` instead")
  type OpAuxFloat[O <: Op,    Ret_Out <: XFloat]    = OpFloat.Aux[O, Ret_Out]
  @deprecated("Please use `OpDoubleAux` instead")
  type OpAuxDouble[O <: Op,   Ret_Out <: XDouble]   = OpDouble.Aux[O, Ret_Out]
  @deprecated("Please use `OpStringAux` instead")
  type OpAuxString[O <: Op,   Ret_Out <: XString]   = OpString.Aux[O, Ret_Out]
  @deprecated("Please use `OpBooleanAux` instead")
  type OpAuxBoolean[O <: Op,  Ret_Out <: XBoolean]  = OpBoolean.Aux[O, Ret_Out]
  /////////////////////////////////////////////////

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
  type ITE[Cond,TBody,EBody]= OpMacro[OpId.ITE, (Cond, TBody, EBody)]
  type ==>[A,B]             = OpMacro[OpId.==>, (A, B)]
  /////////////////////////////////////////////////

  protected[singleton] type Arg[Num, T, TWide] = OpMacro[OpId.Arg, (Num, T, TWide)] //Argument for real-time function creation
  protected[singleton] type GetType[Sym] = OpMacro[OpId.GetType, Tuple1[Sym]] //Argument for real-time function creation
  type AcceptNonLiteral[P1] = OpMacro[OpId.AcceptNonLiteral, Tuple1[P1]]
  type GetArg[ArgIdx]       = OpMacro[OpId.GetArg, (ArgIdx, False)] //Use to get argument type of class/definition
  type GetLHSArg[ArgIdx]    = OpMacro[OpId.GetArg, (ArgIdx, True)] //Use to get argument type of the left-hand-side
  type ImplicitFound[Sym]   = OpMacro[OpId.ImplicitFound, Tuple1[GetType[Sym]]] //Implicit Found boolean indication
  type EnumCount[Sym]       = OpMacro[OpId.EnumCount, Tuple1[GetType[Sym]]] //Number of direct subclasses
  object GetArg {
    type Aux[ArgIdx, Out] = OpGenAux[GetArg[ArgIdx], Out]
  }
  object GetLHSArg {
    type Aux[ArgIdx, Out] = OpGenAux[GetLHSArg[ArgIdx], Out]
  }
  type GetArg0               = GetArg[W.`0`.T]
  object GetArg0 {
    type Aux[Out] = OpGenAux[GetArg0, Out]
  }
  type GetLHSArg0            = GetLHSArg[W.`0`.T]
  object GetLHSArg0 {
    type Aux[Out] = OpGenAux[GetLHSArg0, Out]
  }
  type Id[P1]                = OpMacro[OpId.Id, Tuple1[P1]]
  type ![P1]                 = OpMacro[OpId.!, Tuple1[P1]]
  type Require[Cond]         = OpMacro[OpId.Require, (Cond, DefaultRequireMsg, NoSym)]
  type RequireMsg[Cond,Msg]  = OpMacro[OpId.Require, (Cond, Msg, NoSym)]
  type RequireMsgSym[Cond,Msg,Sym] = OpMacro[OpId.Require, (Cond, Msg, GetType[Sym])]
  type Warn                  = impl.Warn
  type ToNat[P1]             = OpMacro[OpId.ToNat, Tuple1[P1]]
  type ToChar[P1]            = OpMacro[OpId.ToChar, Tuple1[P1]]
  type ToInt[P1]             = OpMacro[OpId.ToInt, Tuple1[P1]]
  type ToLong[P1]            = OpMacro[OpId.ToLong, Tuple1[P1]]
  type ToFloat[P1]           = OpMacro[OpId.ToFloat, Tuple1[P1]]
  type ToDouble[P1]          = OpMacro[OpId.ToDouble, Tuple1[P1]]
  type ToString[P1]          = OpMacro[OpId.ToString, Tuple1[P1]]
  type IsNat[P1]             = OpMacro[OpId.IsNat, Tuple1[P1]]
  type IsChar[P1]            = OpMacro[OpId.IsChar, Tuple1[P1]]
  type IsInt[P1]             = OpMacro[OpId.IsInt, Tuple1[P1]]
  type IsLong[P1]            = OpMacro[OpId.IsLong, Tuple1[P1]]
  type IsFloat[P1]           = OpMacro[OpId.IsFloat, Tuple1[P1]]
  type IsDouble[P1]          = OpMacro[OpId.IsDouble, Tuple1[P1]]
  type IsString[P1]          = OpMacro[OpId.IsString, Tuple1[P1]]
  type IsBoolean[P1]         = OpMacro[OpId.IsBoolean, Tuple1[P1]]
  type IsNonLiteral[P1]      = OpMacro[OpId.IsNonLiteral, Tuple1[P1]]
  type Reverse[P1]           = OpMacro[OpId.Reverse, Tuple1[P1]]
  type Negate[P1]            = OpMacro[OpId.Negate, Tuple1[P1]]
  type NumberOfLeadingZeros[P1] = OpMacro[OpId.NumberOfLeadingZeros, Tuple1[P1]]

  type +[L, R]               = OpMacro[OpId.+, (L, R)]
  type -[L, R]               = OpMacro[OpId.-, (L, R)]
  type *[L, R]               = OpMacro[OpId.*, (L, R)]
  type /[L, R]               = OpMacro[OpId./, (L, R)]
  type %[L, R]               = OpMacro[OpId.%, (L, R)]
  type <[L, R]               = OpMacro[OpId.<, (L, R)]
  type <=[L, R]              = OpMacro[OpId.<=, (L, R)]
  type >=[L, R]              = OpMacro[OpId.>=, (L, R)]
  type >[L, R]               = OpMacro[OpId.>, (L, R)]
  type ==[L, R]              = OpMacro[OpId.==, (L, R)]
  type !=[L, R]              = OpMacro[OpId.!=, (L, R)]
  type &&[L, R]              = OpMacro[OpId.&&, (L, R)]
  type ||[L, R]              = OpMacro[OpId.||, (L, R)]
  type SubSequence[S, IBeg, IEnd] = OpMacro[OpId.SubSequence, (S, IBeg, IEnd)]
  type Substring[S, I]       = OpMacro[OpId.Substring, (S, I)]
  type StartsWith[S, Prefix] = OpMacro[OpId.StartsWith, (S, Prefix)]
  type EndsWith[S, Suffix]   = OpMacro[OpId.EndsWith, (S, Suffix)]
  type Head[S]               = OpMacro[OpId.Head, Tuple1[S]]
  type Tail[S]               = OpMacro[OpId.Tail, Tuple1[S]]
  type Length[S]             = OpMacro[OpId.Length, Tuple1[S]]
  type CharAt[S, I]          = OpMacro[OpId.CharAt, (S, I)]
  type Matches[S, Regex]     = OpMacro[OpId.Matches, (S, Regex)]
  type FirstMatch[S, Regex]  = OpMacro[OpId.FirstMatch, (S, Regex)]
  type PrefixMatch[S, Regex] = OpMacro[OpId.PrefixMatch, (S, Regex)]
  type ReplaceFirstMatch[S, Regex, R] = OpMacro[OpId.ReplaceFirstMatch, (S, Regex, R)]
  type ReplaceAllMatches[S, Regex, R] = OpMacro[OpId.ReplaceAllMatches, (S, Regex, R)]

  type CompileTime[C]       = Require[ITE[IsNonLiteral[C], W.`true`.T, C]]
  type RunTime[R]           = SafeBoolean[IsNonLiteral[R]]

  object math {
    type Pi                 = W.`3.141592653589793`.T
    type E                  = W.`2.718281828459045`.T
    type Abs[P1]            = OpMacro[OpId.Abs, Tuple1[P1]]
    type Min[L, R]          = OpMacro[OpId.Min, (L, R)]
    type Max[L, R]          = OpMacro[OpId.Max, (L, R)]
    type Pow[L, R]          = OpMacro[OpId.Pow, (L, R)]
    type Floor[P1]          = OpMacro[OpId.Floor, Tuple1[P1]]
    type Ceil[P1]           = OpMacro[OpId.Ceil, Tuple1[P1]]
    type Round[P1]          = OpMacro[OpId.Round, Tuple1[P1]]
    type Sin[P1]            = OpMacro[OpId.Sin, Tuple1[P1]]
    type Cos[P1]            = OpMacro[OpId.Cos, Tuple1[P1]]
    type Tan[P1]            = OpMacro[OpId.Tan, Tuple1[P1]]
    type Sqrt[P1]           = OpMacro[OpId.Sqrt, Tuple1[P1]]
    type Log[P1]            = OpMacro[OpId.Log, Tuple1[P1]]
    type Log10[P1]          = OpMacro[OpId.Log10, Tuple1[P1]]
  }


  /////////////////////////////////////////////////
  //Implicit proof that
  //singleton type is equivalent to type operation
  //E.g. val four : 2 + 2 = 4
  /////////////////////////////////////////////////
  implicit def singletonToOp[
  X <: Singleton, S, A, M, OP_OUT
  ](x: X)
   (implicit
    op : OpMacro[S, A],
    opaux: OpGenAux[OpMacro[S, A], OP_OUT],
    check : Require[X == OP_OUT]
   ) : OpMacro[S, A] = op
  /////////////////////////////////////////////////


  /////////////////////////////////////////////////
  //Implicit proof that
  //type operation is equivalent to singleton type
  //E.g. val four : 4 = implicitly[2 + 2]
  /////////////////////////////////////////////////
  implicit def opToSingleton[
  S, A, OP_OUT
  ](op : OpMacro[S, A])
   (implicit
    opaux: OpGenAux[OpMacro[S, A], OP_OUT],
    id : Id[OP_OUT]
   ) : OP_OUT = id.value.asInstanceOf[OP_OUT]
  /////////////////////////////////////////////////


  /////////////////////////////////////////////////
  //Implicit proof that
  //type operation is equivalent to a different type operation
  //E.g. val four : 1 + 3 = implicitly[2 + 2]
  /////////////////////////////////////////////////
  implicit def opToOp[
  S1, A1,
  S2, A2,
  OP_OUT1,
  OP_OUT2
  ](opA : OpMacro[S1, A1])
   (implicit
    opauxA: OpGenAux[OpMacro[S1, A1], OP_OUT1],
    opauxB: OpGenAux[OpMacro[S2, A2], OP_OUT2],
    opB : OpMacro[S2, A2],
    check : Require[OP_OUT1 == OP_OUT2]
  ) : OpMacro[S2, A2] = opB
  /////////////////////////////////////////////////
}
