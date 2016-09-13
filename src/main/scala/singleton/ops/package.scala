package singleton
import singleton.ops.impl._

package object ops {
  val W = shapeless.Witness
  type +[P1, P2] = Ret[SingletonTypeFunc2["+",@@[P1], @@[P2]]]
  type @@[S] = SingletonTypeValue[S]
  type Ret[S <: SingletonTypeExpr] = Return[S]
}
