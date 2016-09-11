package singleton
import singleton.ops.impl._

package object ops {
  val W = shapeless.Witness
  type +[P1, P2] = Sum2[@@[P1], @@[P2]]
  type @@[S] = SingletonTypeValue[S]

}
