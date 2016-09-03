package singleton.ops.impl

trait Op2CompanionGen[F[FT, FA <: FT with Singleton, FB <: FT with Singleton] <: Op2[FT, FA, FB]] {
  type Aux[T, A <: T with Singleton, B <: T with Singleton, Out0 <: T with Singleton] = F[T, A, B]// { type Out = Out0 }

  def apply[T, A <: T with Singleton, B <: T with Singleton](implicit ev: F[T, A, B]): Aux[T, A, B, ev.Out] = ev
}
