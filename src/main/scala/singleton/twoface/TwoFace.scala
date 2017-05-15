package singleton.twoface

import impl._

object TwoFace {
  final class Char[T] private(val value : scala.Char) extends AnyVal with TwoFaceAny.Char[T] {
    @inline def getValue : scala.Char = value
  }
  implicit object Char extends TwoFaceAny.Builder[Char, scala.Char] {
    protected[twoface] def create[T](value : scala.Char) = new Char[T](value)
  }
  final class Int[T] private(val value : scala.Int) extends AnyVal with TwoFaceAny.Int[T] {
    @inline def getValue : scala.Int = value
  }
  implicit object Int extends TwoFaceAny.Builder[Int, scala.Int] {
    protected[twoface] def create[T](value : scala.Int) = new Int[T](value)
  }
  final class Long[T] private(val value : scala.Long) extends AnyVal with TwoFaceAny.Long[T] {
    @inline def getValue : scala.Long = value
  }
  implicit object Long extends TwoFaceAny.Builder[Long, scala.Long] {
    protected[twoface] def create[T](value : scala.Long) = new Long[T](value)
  }
  final class Float[T] private(val value : scala.Float) extends AnyVal with TwoFaceAny.Float[T] {
    @inline def getValue : scala.Float = value
  }
  implicit object Float extends TwoFaceAny.Builder[Float, scala.Float] {
    protected[twoface] def create[T](value : scala.Float) = new Float[T](value)
  }
  final class Double[T] private(val value : scala.Double) extends AnyVal with TwoFaceAny.Double[T] {
    @inline def getValue : scala.Double = value
  }
  implicit object Double extends TwoFaceAny.Builder[Double, scala.Double] {
    protected[twoface] def create[T](value : scala.Double) = new Double[T](value)
  }
  final class String[T] private(val value : java.lang.String) extends AnyVal with TwoFaceAny.String[T] {
    @inline def getValue : java.lang.String = value
  }
  implicit object String extends TwoFaceAny.Builder[String, java.lang.String] {
    protected[twoface] def create[T](value : java.lang.String) = new String[T](value)
  }
  final class Boolean[T] private(val value : scala.Boolean) extends AnyVal with TwoFaceAny.Boolean[T] {
    @inline def getValue : scala.Boolean = value
  }
  implicit object Boolean extends TwoFaceAny.Builder[Boolean, scala.Boolean] {
    protected[twoface] def create[T](value : scala.Boolean) = new Boolean[T](value)
  }
}

