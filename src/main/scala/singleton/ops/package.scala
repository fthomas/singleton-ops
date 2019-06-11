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
  type XSymbol              = Symbol with Singleton
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
  type SafeSymbol[P1]       = impl.OpSymbol[ToSymbol[Require[IsSymbol[P1]] ==> P1]]
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
  type OpAuxSymbol[O <: Op,   Ret_Out <: Symbol]    = OpSymbol.Aux[O, Ret_Out]
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
  type GetArg[ArgIdx]       = OpMacro[OpId.GetArg, ArgIdx, NP, NP] //Use to get argument type of class/definition
  type GetLHSArg[ArgIdx]    = OpMacro[OpId.GetLHSArg, ArgIdx, NP, NP] //Use to get argument type of the left-hand-side
  type ImplicitFound[Sym]   = OpMacro[OpId.ImplicitFound, GetType[Sym], NP, NP] //Implicit Found boolean indication
  type EnumCount[Sym]       = OpMacro[OpId.EnumCount, GetType[Sym], NP, NP] //Number of direct subclasses
  final val  GetArg         = impl.GetArg
  final val  GetLHSArg      = impl.GetLHSArg
  type GetArg0              = GetArg[W.`0`.T]
  type GetLHSArg0           = GetLHSArg[W.`0`.T]
  type Id[P1]               = OpMacro[OpId.Id, P1, NP, NP]
  type ![P1]                = OpMacro[OpId.!, P1, NP, NP]
  type Require[Cond]        = OpMacro[OpId.Require, Cond, DefaultRequireMsg, NP]
  type RequireMsg[Cond,Msg] = OpMacro[OpId.Require, Cond, Msg, NP]
  type RequireMsgSym[Cond,Msg,Sym] = OpMacro[OpId.Require, Cond, Msg, GetType[Sym]]
  type Warn                 = impl.Warn
  type ToNat[P1]            = OpMacro[OpId.ToNat, P1, NP, NP]
  type ToChar[P1]           = OpMacro[OpId.ToChar, P1, NP, NP]
  type ToInt[P1]            = OpMacro[OpId.ToInt, P1, NP, NP]
  type ToLong[P1]           = OpMacro[OpId.ToLong, P1, NP, NP]
  type ToFloat[P1]          = OpMacro[OpId.ToFloat, P1, NP, NP]
  type ToDouble[P1]         = OpMacro[OpId.ToDouble, P1, NP, NP]
  type ToString[P1]         = OpMacro[OpId.ToString, P1, NP, NP]
  type ToSymbol[P1]         = OpMacro[OpId.ToSymbol, P1, NP, NP]
  type IsNat[P1]            = OpMacro[OpId.IsNat, P1, NP, NP]
  type IsChar[P1]           = OpMacro[OpId.IsChar, P1, NP, NP]
  type IsInt[P1]            = OpMacro[OpId.IsInt, P1, NP, NP]
  type IsLong[P1]           = OpMacro[OpId.IsLong, P1, NP, NP]
  type IsFloat[P1]          = OpMacro[OpId.IsFloat, P1, NP, NP]
  type IsDouble[P1]         = OpMacro[OpId.IsDouble, P1, NP, NP]
  type IsString[P1]         = OpMacro[OpId.IsString, P1, NP, NP]
  type IsBoolean[P1]        = OpMacro[OpId.IsBoolean, P1, NP, NP]
  type IsSymbol[P1]         = OpMacro[OpId.IsSymbol, P1, NP, NP]
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
  type Substring[P1, P2]    = OpMacro[OpId.Substring, P1, P2, NP]
  type Length[P1]           = OpMacro[OpId.Length, P1, NP, NP]
  type CharAt[P1, P2]       = OpMacro[OpId.CharAt, P1, P2, NP]

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


  /////////////////////////////////////////////////
  //Implicit proof for type class containers of
  //singleton type operations & literals
  /////////////////////////////////////////////////
  implicit def opContainer1[
  F,
  T,
  C[_]
  ](cf : C[F])
   (implicit
    ct : C[T],
    check : Require[
      F == T
      ])
  : C[T] = ct

  implicit def opContainer2[
  F1, F2,
  T1, T2,
  C[_,_]
  ](cf : C[F1, F2])
   (implicit
    ct : C[T1, T2],
    check : Require[
        (F1 == T1) &&
        (F2 == T2)
      ]
   ) : C[T1, T2] = ct

  implicit def opContainer3[
  F1, F2, F3,
  T1, T2, T3,
  C[_,_,_]
  ](cf : C[F1, F2, F3])
   (implicit
    ct : C[T1, T2, T3],
    check : Require[
        (F1 == T1) &&
        (F2 == T2) &&
        (F3 == T3)
      ]
   ) : C[T1, T2, T3] = ct
  /////////////////////////////////////////////////
}
