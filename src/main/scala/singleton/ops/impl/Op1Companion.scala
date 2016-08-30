package singleton.ops.impl

trait Op1Companion[F[_] <: Op] {
  type Aux[A, Out0] = F[A] { type Out = Out0 }

  def apply[A](implicit ev: F[A]): Aux[A, ev.Out] = ev
}

trait Op1CompanionGen[F[_, _] <: Op] {
  type Aux[T, A, Out0] = F[T, A] { type Out = Out0 }

  def apply[T, A](implicit ev: F[T, A]): Aux[T, A, ev.Out] = ev
}
