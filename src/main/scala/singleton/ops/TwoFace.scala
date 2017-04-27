package singleton.ops

sealed abstract class TwoFace[Face, T](val value : Face) {
  def isLiteral(implicit rt : RunTime[T]) : Boolean = !rt
  override def toString = value.toString
}
sealed trait TwoFaceObj[TF[_], Face] {
  protected[ops] def create[T](value : Face) : TF[T]
  protected[ops] def op[T](op : => Face)(implicit fb : FallBack[Face, T]) : TF[fb.Out] =
    create[fb.Out](if (fb.isLiteral) fb.value.get else op)
  def apply[T](value : Face)(implicit fb : FallBack[Face, T]) : TF[fb.Out] =
    create[fb.Out](value)
  implicit def safe[T <: Face with Singleton](value : T) : TF[T] = create[T](value)
  implicit def unsafe[T <: Face](value : T) : TF[Face] = create[Face](value)
  implicit def ev[T](implicit si : SafeCast[Face with Singleton, T]) : TF[si.Out] = create[si.Out](si.value)
}


final class TwoFaceInt[T] private(value : Int) extends TwoFace[Int, T](value) {
  def + [R](r : TwoFaceInt[R])(implicit fb : FallBack[Int, T + R]) = TwoFaceInt.op[T + R](this.value + r.value)
  def - [R](r : TwoFaceInt[R])(implicit fb : FallBack[Int, T - R]) = TwoFaceInt.op[T - R](this.value - r.value)
  def * [R](r : TwoFaceInt[R])(implicit fb : FallBack[Int, T * R]) = TwoFaceInt.op[T * R](this.value * r.value)
  def / [R](r : TwoFaceInt[R])(implicit fb : FallBack[Int, T / R]) = TwoFaceInt.op[T / R](this.value / r.value)
}

object TwoFaceInt extends TwoFaceObj[TwoFaceInt, Int] {
  protected[ops] def create[T](value : Int) = new TwoFaceInt[T](value)
}

