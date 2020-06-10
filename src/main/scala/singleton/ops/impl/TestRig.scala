package singleton.ops.impl

import singleton.ops._

trait Rig[V <: XInt] {
  val value: Int
  override def toString: String = s"Rig($value)"
}

object Rig {
  // get the 'Out' type resulting from resolving an implicit type
  object impeval {
    trait Out[O]
    def apply[I <: { type Out } ](implicit i: I): Out[i.Out] = new Out[i.Out] {}
  }

  implicit def rigAddition[L <: Rig[_], R <: Rig[_], O <: Rig[_]](implicit
    add: Add.Aux[L, R, O]): OpIntercept.Aux[OpId.+, L, R, W.`0`.T, O] =
      new OpIntercept[OpId.+, L, R, W.`0`.T] {
        type Out = O
        val value = add.value
      }

  trait Add[L, R] {
    type Out
    val value: Out
  }
  object Add {
    type Aux[L, R, O] = Add[L, R] { type Out = O }

    implicit def addRig[LV <: XInt, RV <: XInt, OV <: XInt](implicit
        add: OpInt.Aux[LV + RV, OV]): Add.Aux[Rig[LV], Rig[RV], Rig[OV]] =
      new Add[Rig[LV], Rig[RV]] {
        type Out = Rig[OV]
        val value = new Rig[OV] { val value = add.value }
      }
  }
}

