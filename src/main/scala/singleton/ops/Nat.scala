package singleton.ops

import shapeless._
import singleton.ops.impl._

trait ToNat[P1 <: SingletonTypeExpr] extends Nat //Serializable {type Out <: Nat}

object ToNat {
  type Aux[
  P1 <: SingletonTypeExpr,
  Ret_Out <: Nat
  ] = ToNat[P1] {
    type N = Ret_Out
  }

  implicit def impl[
  P1 <: SingletonTypeExpr,
  P1_BaseType <: Int,
  P1_Out <: P1_BaseType with Singleton,
  Ret_Out <: Nat
  ](implicit p1_ret: Repeater.Aux[P1, P1_BaseType, P1_Out],
    op: ToNatMacro[P1_Out])
  : Aux[P1, op.N] = new ToNat[P1] { type N = op.N }
}
