package singleton
import singleton.ops.impl._

package object ops {
  type $Int                 = Int with Singleton
  type $Long                = Long with Singleton
  type $Double              = Double with Singleton
  type $String              = String with Singleton
  type $Boolean             = Boolean with Singleton
  type ITE[Cond,TBody,EBody]= OpMacro["ITE",Cond, TBody, EBody]
  type While[Cond,Body,Ret] = OpMacro["!",Cond, Body, Ret]
  type ==>[A,B]             = OpMacro["==>",A, B, 0]
  type SV[Name,Value]       = OpMacro["SV",Name, Value, 0]
  type GV[Name]             = OpMacro["GV",Name, 0, 0]

  type ![P1]                = OpMacro["!",P1, 0, 0]
  type Require[P1]          = OpMacro["Require",P1, 0, 0]
  type ToNat[P1]            = OpMacro["ToNat",P1, 0, 0]
  type ToInt[P1]            = OpMacro["ToInt",P1, 0, 0]
  type ToLong[P1]           = OpMacro["ToLong",P1, 0, 0]
  type ToDouble[P1]         = OpMacro["ToDouble",P1, 0, 0]
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
}
