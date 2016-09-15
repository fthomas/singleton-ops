package singleton
import singleton.ops.impl._

package object ops {
  type ![P1]                = Op1Macro["!",P1]
  type Require[P1]          = Op1Macro["Require",P1]
  type ToNat[P1]            = Op1Macro["ToNat",P1]
  type ToInt[P1]            = Op1Macro["ToInt",P1]
  type ToLong[P1]           = Op1Macro["ToLong",P1]
  type ToDouble[P1]         = Op1Macro["ToDouble",P1]
  type Reverse[P1]          = Op1Macro["Reverse",P1]
  type Negate[P1]           = Op1Macro["Negate",P1]
  type Abs[P1]              = Op1Macro["Abs",P1]

  type +[P1, P2]            = Op2Macro["+",P1, P2]
  type -[P1, P2]            = Op2Macro["-",P1, P2]
  type *[P1, P2]            = Op2Macro["*",P1, P2]
  type /[P1, P2]            = Op2Macro["/",P1, P2]
  type <[P1, P2]            = Op2Macro["<",P1, P2]
  type <=[P1, P2]           = Op2Macro["<=",P1, P2]
  type >=[P1, P2]           = Op2Macro[">=",P1, P2]
  type >[P1, P2]            = Op2Macro[">",P1, P2]
  type ==[P1, P2]           = Op2Macro["==",P1, P2]
  type !=[P1, P2]           = Op2Macro["!=",P1, P2]
  type &&[P1, P2]           = Op2Macro["&&",P1, P2]
  type ||[P1, P2]           = Op2Macro["||",P1, P2]
  type Min[P1, P2]          = Op2Macro["Min",P1, P2]
  type Max[P1, P2]          = Op2Macro["Max",P1, P2]
  type Substring[P1, P2]    = Op2Macro["Substring",P1, P2]
}
