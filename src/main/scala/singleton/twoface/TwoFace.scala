package singleton.twoface

import singleton.ops._

object TwoFace {
  sealed trait TwoFaceAny[Face, T] extends Any {
    def isLiteral(implicit rt : RunTime[T]) : Boolean = !rt
    @inline def getValue : Face
    override def toString = getValue.toString
  }

  sealed trait TwoFaceObj[TF[_], Face] {
    protected[twoface] def create[T](value : Face) : TF[T]
    protected[twoface] def op[T](op : => Face)(implicit fb : FallBack[Face, T]) : TF[fb.Out] =
      create[fb.Out](if (fb.isLiteral) fb.value.get else op)
    def apply[T](value : Face)(implicit fb : FallBack[Face, T]) : TF[fb.Out] =
      create[fb.Out](value)
    implicit def safe[T <: Face with Singleton](value : T) : TF[T] = create[T](value)
    implicit def unsafe[T <: Face](value : T) : TF[Face] = create[Face](value)
    implicit def ev[T](implicit si : SafeCast[Face with Singleton, T]) : TF[si.Out] = create[si.Out](si.value)
  }

  final class Char[T] private(val value : scala.Char) extends AnyVal with TwoFaceAny[scala.Char, T] {
    def + [R](r : Char[R])(implicit fb : FallBack[scala.Char, T + R]) = Int.op[T + R](this.getValue + r.getValue)
    def - [R](r : Char[R])(implicit fb : FallBack[scala.Char, T - R]) = Int.op[T - R](this.getValue - r.getValue)
    def * [R](r : Char[R])(implicit fb : FallBack[scala.Char, T * R]) = Int.op[T * R](this.getValue * r.getValue)
    def / [R](r : Char[R])(implicit fb : FallBack[scala.Char, T / R]) = Int.op[T / R](this.getValue / r.getValue)
    def + [R](r : Int[R])(implicit fb : FallBack[scala.Int, T + R]) = Int.op[T + R](this.getValue + r.getValue)
    def - [R](r : Int[R])(implicit fb : FallBack[scala.Int, T - R]) = Int.op[T - R](this.getValue - r.getValue)
    def * [R](r : Int[R])(implicit fb : FallBack[scala.Int, T * R]) = Int.op[T * R](this.getValue * r.getValue)
    def / [R](r : Int[R])(implicit fb : FallBack[scala.Int, T / R]) = Int.op[T / R](this.getValue / r.getValue)
    @inline def getValue : scala.Char = value
  }

  object Char extends TwoFaceObj[Char, scala.Char] {
    protected[twoface] def create[T](value : scala.Char) = new Char[T](value)
  }

  final class Int[T] private(val value : scala.Int) extends AnyVal with TwoFaceAny[scala.Int, T] {
    def + [R](r : Char[R])(implicit fb : FallBack[scala.Char, T + R]) = Int.op[T + R](this.getValue + r.getValue)
    def - [R](r : Char[R])(implicit fb : FallBack[scala.Char, T - R]) = Int.op[T - R](this.getValue - r.getValue)
    def * [R](r : Char[R])(implicit fb : FallBack[scala.Char, T * R]) = Int.op[T * R](this.getValue * r.getValue)
    def / [R](r : Char[R])(implicit fb : FallBack[scala.Char, T / R]) = Int.op[T / R](this.getValue / r.getValue)
    def + [R](r : Int[R])(implicit fb : FallBack[scala.Int, T + R]) = Int.op[T + R](this.getValue + r.getValue)
    def - [R](r : Int[R])(implicit fb : FallBack[scala.Int, T - R]) = Int.op[T - R](this.getValue - r.getValue)
    def * [R](r : Int[R])(implicit fb : FallBack[scala.Int, T * R]) = Int.op[T * R](this.getValue * r.getValue)
    def / [R](r : Int[R])(implicit fb : FallBack[scala.Int, T / R]) = Int.op[T / R](this.getValue / r.getValue)
    @inline def getValue : scala.Int = value
  }

  object Int extends TwoFaceObj[Int, scala.Int] {
    protected[twoface] def create[T](value : scala.Int) = new Int[T](value)
  }

}


