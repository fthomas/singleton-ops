package singleton.ops

import impl._

import shapeless._

//trait FromNat[N <: Nat] extends SingletonTypeExprBase[Int]
//
//object FromNat {
//  def apply[N <: Nat](implicit fromNat: FromNat[N]): FromNat[N] = fromNat
//
//  implicit val fromNat0 : FromNat[_0] {type OutInt = 0; type Out = 0} = new FromNat[_0] {
//    type OutInt = 0
//    type Out = 0
//    val value : Out = 0
//  }
//  implicit def fromNatSucc[N <: Nat](implicit fromNat : FromNat[N] + 1) : FromNat[Succ[N]]
//    {type OutInt = fromNat.OutInt; type Out = fromNat.OutInt} = new FromNat[Succ[N]] {
//    type OutInt = fromNat.OutInt
//    type Out = fromNat.OutInt
//    val value : Out = fromNat.value.asInstanceOf[Out]
//  }
//}
