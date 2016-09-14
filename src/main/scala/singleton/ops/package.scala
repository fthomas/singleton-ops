package singleton
import singleton.ops.impl._

package object ops {
//  val W = shapeless.Witness
  type @@[S] = SingletonTypeValue[S]
  type Ret[S <: SingletonTypeExpr] = Return[S]
//  type ToNat[P1 <: SingletonTypeExpr] = impl.ToNat[@@[P1]]

  type +[P1, P2]            = SingletonTypeFunc2["+",@@[P1], @@[P2]]
  type -[P1, P2]            = SingletonTypeFunc2["-",@@[P1], @@[P2]]
  type *[P1, P2]            = SingletonTypeFunc2["*",@@[P1], @@[P2]]
  type /[P1, P2]            = SingletonTypeFunc2["/",@@[P1], @@[P2]]
  type <[P1, P2]            = SingletonTypeFunc2["<",@@[P1], @@[P2]]
  type <=[P1, P2]           = SingletonTypeFunc2["<=",@@[P1], @@[P2]]
  type >=[P1, P2]           = SingletonTypeFunc2[">=",@@[P1], @@[P2]]
  type >[P1, P2]            = SingletonTypeFunc2[">",@@[P1], @@[P2]]
  type ==[P1, P2]           = SingletonTypeFunc2["==",@@[P1], @@[P2]]
  type !=[P1, P2]           = SingletonTypeFunc2["!=",@@[P1], @@[P2]]
  type &&[P1, P2]           = SingletonTypeFunc2["&&",@@[P1], @@[P2]]
  type ||[P1, P2]           = SingletonTypeFunc2["||",@@[P1], @@[P2]]
  type Min[P1, P2]          = SingletonTypeFunc2["Min",@@[P1], @@[P2]]
  type Max[P1, P2]          = SingletonTypeFunc2["Max",@@[P1], @@[P2]]
  type Substring[P1, P2]    = SingletonTypeFunc2["Substring",@@[P1], @@[P2]]

  type ![P1]                = SingletonTypeFunc1["!",@@[P1]]
  type Require[P1]          = SingletonTypeFunc1["Require",@@[P1]]
  type ToInt[P1]            = SingletonTypeFunc1["ToInt",@@[P1]]
  type ToLong[P1]           = SingletonTypeFunc1["ToLong",@@[P1]]
  type ToDouble[P1]         = SingletonTypeFunc1["ToDouble",@@[P1]]
  type Reverse[P1]          = SingletonTypeFunc1["Reverse",@@[P1]]
  type Negate[P1]           = SingletonTypeFunc1["Negate",@@[P1]]
  type Abs[P1]              = SingletonTypeFunc1["Abs",@@[P1]]
}
