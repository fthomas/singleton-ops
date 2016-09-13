package singleton
import singleton.ops.impl._

package object ops {
  val W = shapeless.Witness
  type @@[S] = SingletonTypeValue[S]
  type Ret[S <: SingletonTypeExpr] = Return[S]

  type +[P1, P2]            = Ret[SingletonTypeFunc2["+",@@[P1], @@[P2]]]
  type *[P1, P2]            = Ret[SingletonTypeFunc2["*",@@[P1], @@[P2]]]
  type Substring[P1, P2]    = Ret[SingletonTypeFunc2["Substring",@@[P1], @@[P2]]]

  type ToInt[P1]            = Ret[SingletonTypeFunc1["ToInt",@@[P1]]]
  type ToLong[P1]           = Ret[SingletonTypeFunc1["ToLong",@@[P1]]]
  type ToDouble[P1]         = Ret[SingletonTypeFunc1["ToDouble",@@[P1]]]
  type Reverse[P1]          = Ret[SingletonTypeFunc1["Reverse",@@[P1]]]
  type Negate[P1]           = Ret[SingletonTypeFunc1["Negate",@@[P1]]]
}
