package singleton.ops.impl

trait Op2Companion[F[_, _] <: Op] {
  type Aux[A, B, Out0] = F[A, B] { type Out = Out0 }

  def apply[A, B](implicit ev: F[A, B]): Aux[A, B, ev.Out] = ev
}
