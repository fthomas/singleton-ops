package singleton
import singleton.ops.impl._

package object ops {
  val W = shapeless.Witness
  type @@[S] = SingletonTypeValue[S]
  type Ret[S <: SingletonTypeExpr] = Return[S]

  type +[P1, P2] = Ret[SingletonTypeFunc2["+",@@[P1], @@[P2]]]

  type ToLong[P1] = Ret[SingletonTypeFunc1["ToLong",@@[P1]]]
  type ToInt[P1]  = Ret[SingletonTypeFunc1["ToInt",@@[P1]]]
}
