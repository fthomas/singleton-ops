package singleton
import singleton.ops.impl._

package object ops {
  val W = shapeless.Witness
  type +[P1 <: SingletonTypeExpr, P2 <: SingletonTypeExpr] = Sum2[P1, P2]
  type @@[S <: Singleton] = SingletonTypeValue[S]

}
