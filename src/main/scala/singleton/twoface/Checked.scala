package singleton.twoface
import impl._

object Checked {
  @scala.annotation.implicitNotFound("${Msg}")
  final class Char[T, Cond[_,_], Param, Msg] private(val value : scala.Char) extends AnyVal with TwoFaceAny.Char[T] {
    @inline def getValue : scala.Char = value
  }
  object Char extends CheckedAny.Builder[Char, scala.Char] {
    def create[T, Cond[_,_], Param, Msg](value : scala.Char) : Char[T, Cond, Param, Msg] = new Char[T, Cond, Param, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Int[T, Cond[_,_], Param, Msg] private(val value : scala.Int) extends AnyVal with TwoFaceAny.Int[T] {
    @inline def getValue : scala.Int = value
  }
  implicit object Int extends CheckedAny.Builder[Int, scala.Int] {
    def create[T, Cond[_,_], Param, Msg](value : scala.Int) : Int[T, Cond, Param, Msg] = new Int[T, Cond, Param, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Long[T, Cond[_,_], Param, Msg] private(val value : scala.Long) extends AnyVal with TwoFaceAny.Long[T] {
    @inline def getValue : scala.Long = value
  }
  object Long extends CheckedAny.Builder[Long, scala.Long] {
    def create[T, Cond[_,_], Param, Msg](value : scala.Long) : Long[T, Cond, Param, Msg] = new Long[T, Cond, Param, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Float[T, Cond[_,_], Param, Msg] private(val value : scala.Float) extends AnyVal with TwoFaceAny.Float[T] {
    @inline def getValue : scala.Float = value
  }
  object Float extends CheckedAny.Builder[Float, scala.Float] {
    def create[T, Cond[_,_], Param, Msg](value : scala.Float) : Float[T, Cond, Param, Msg] = new Float[T, Cond, Param, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Double[T, Cond[_,_], Param, Msg] private(val value : scala.Double) extends AnyVal with TwoFaceAny.Double[T] {
    @inline def getValue : scala.Double = value
  }
  object Double extends CheckedAny.Builder[Double, scala.Double] {
    def create[T, Cond[_,_], Param, Msg](value : scala.Double) : Double[T, Cond, Param, Msg] = new Double[T, Cond, Param, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class String[T, Cond[_,_], Param, Msg] private(val value : java.lang.String) extends AnyVal with TwoFaceAny.String[T] {
    @inline def getValue : java.lang.String = value
  }
  object String extends CheckedAny.Builder[String, java.lang.String] {
    def create[T, Cond[_,_], Param, Msg](value : java.lang.String) : String[T, Cond, Param, Msg] = new String[T, Cond, Param, Msg](value)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Boolean[T, Cond[_,_], Param, Msg] private(val value : scala.Boolean) extends AnyVal with TwoFaceAny.Boolean[T] {
    @inline def getValue : scala.Boolean = value
  }
  object Boolean extends CheckedAny.Builder[Boolean, scala.Boolean] {
    def create[T, Cond[_,_], Param, Msg](value : scala.Boolean) : Boolean[T, Cond, Param, Msg] = new Boolean[T, Cond, Param, Msg](value)
  }
}
