package singleton.ops

trait Op1Companion[F[_] <: Op] {
  type Aux[A, Out0] = F[A] { type Out = Out0 }

  def apply[A](implicit ev: F[A]): Aux[A, ev.Out] = ev
}
