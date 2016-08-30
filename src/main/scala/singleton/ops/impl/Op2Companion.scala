package singleton.ops.impl

trait Op2Companion[F[_, _] <: Op] {
  type Aux[A, B, Out0] = F[A, B] { type Out = Out0 }

  def apply[A, B](implicit ev: F[A, B]): Aux[A, B, ev.Out] = ev
}

trait Op2CompanionGen[F[_, _, _] <: Op] {
  type Aux[T, A, B, Out0] = F[T, A, B] { type Out = Out0 }

  def apply[T, A, B](implicit ev: F[T, A, B]): Aux[T, A, B, ev.Out] = ev
}
