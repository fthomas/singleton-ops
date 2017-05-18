package singleton.twoface
import impl._

object Checked {
  @scala.annotation.implicitNotFound("${Msg}")
  final class Char[T, Cond[_], Msg] private(val value : scala.Char) extends AnyVal with TwoFaceAny.Char[T] {
    @inline def getValue : scala.Char = value
  }
  object Char extends CheckedAny.Builder[Char, scala.Char] {
    protected[twoface] def create[T, Cond[_], Msg](value : scala.Char) : Char[T, Cond, Msg] = new Char[T, Cond, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Int[T, Cond[_], Msg] private(val value : scala.Int) extends AnyVal with TwoFaceAny.Int[T] {
    @inline def getValue : scala.Int = value
  }
  implicit object Int extends CheckedAny.Builder[Int, scala.Int] {
    protected[twoface] def create[T, Cond[_], Msg](value : scala.Int) : Int[T, Cond, Msg] = new Int[T, Cond, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Long[T, Cond[_], Msg] private(val value : scala.Long) extends AnyVal with TwoFaceAny.Long[T] {
    @inline def getValue : scala.Long = value
  }
  object Long extends CheckedAny.Builder[Long, scala.Long] {
    protected[twoface] def create[T, Cond[_], Msg](value : scala.Long) : Long[T, Cond, Msg] = new Long[T, Cond, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Float[T, Cond[_], Msg] private(val value : scala.Float) extends AnyVal with TwoFaceAny.Float[T] {
    @inline def getValue : scala.Float = value
  }
  object Float extends CheckedAny.Builder[Float, scala.Float] {
    protected[twoface] def create[T, Cond[_], Msg](value : scala.Float) : Float[T, Cond, Msg] = new Float[T, Cond, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Double[T, Cond[_], Msg] private(val value : scala.Double) extends AnyVal with TwoFaceAny.Double[T] {
    @inline def getValue : scala.Double = value
  }
  object Double extends CheckedAny.Builder[Double, scala.Double] {
    protected[twoface] def create[T, Cond[_], Msg](value : scala.Double) : Double[T, Cond, Msg] = new Double[T, Cond, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class String[T, Cond[_], Msg] private(val value : java.lang.String) extends AnyVal with TwoFaceAny.String[T] {
    @inline def getValue : java.lang.String = value
  }
  object String extends CheckedAny.Builder[String, java.lang.String] {
    protected[twoface] def create[T, Cond[_], Msg](value : java.lang.String) : String[T, Cond, Msg] = new String[T, Cond, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Boolean[T, Cond[_], Msg] private(val value : scala.Boolean) extends AnyVal with TwoFaceAny.Boolean[T] {
    @inline def getValue : scala.Boolean = value
  }
  object Boolean extends CheckedAny.Builder[Boolean, scala.Boolean] {
    protected[twoface] def create[T, Cond[_], Msg](value : scala.Boolean) : Boolean[T, Cond, Msg] = new Boolean[T, Cond, Msg](value)
  }
}
