package singleton.twoface
import impl._

object Checked {
  final class Char[T, Cond[_,_], Param, Msg[_,_]] private(val value : scala.Char) extends AnyVal with CheckedAny[scala.Char, T] with TwoFaceAny.Char[T] {
    @inline def getValue : scala.Char = value
  }
  object Char extends CheckedAny.Builder[Char, scala.Char] {
    def create[T, Cond[_,_], Param, Msg[_,_]](value : scala.Char) : Char[T, Cond, Param, Msg] = new Char[T, Cond, Param, Msg](value)
  }

  final class Int[T, Cond[_,_], Param, Msg[_,_]] private(val value : scala.Int) extends AnyVal with CheckedAny[scala.Int, T] with TwoFaceAny.Int[T] {
    @inline def getValue : scala.Int = value
  }
  object Int extends CheckedAny.Builder[Int, scala.Int] {
    def create[T, Cond[_,_], Param, Msg[_,_]](value : scala.Int) : Int[T, Cond, Param, Msg] = new Int[T, Cond, Param, Msg](value)
  }

  final class Long[T, Cond[_,_], Param, Msg[_,_]] private(val value : scala.Long) extends AnyVal with CheckedAny[scala.Long, T] with TwoFaceAny.Long[T] {
    @inline def getValue : scala.Long = value
  }
  object Long extends CheckedAny.Builder[Long, scala.Long] {
    def create[T, Cond[_,_], Param, Msg[_,_]](value : scala.Long) : Long[T, Cond, Param, Msg] = new Long[T, Cond, Param, Msg](value)
  }

  final class Float[T, Cond[_,_], Param, Msg[_,_]] private(val value : scala.Float) extends AnyVal with CheckedAny[scala.Float, T] with TwoFaceAny.Float[T] {
    @inline def getValue : scala.Float = value
  }
  object Float extends CheckedAny.Builder[Float, scala.Float] {
    def create[T, Cond[_,_], Param, Msg[_,_]](value : scala.Float) : Float[T, Cond, Param, Msg] = new Float[T, Cond, Param, Msg](value)
  }

  final class Double[T, Cond[_,_], Param, Msg[_,_]] private(val value : scala.Double) extends AnyVal with CheckedAny[scala.Double, T] with TwoFaceAny.Double[T] {
    @inline def getValue : scala.Double = value
  }
  object Double extends CheckedAny.Builder[Double, scala.Double] {
    def create[T, Cond[_,_], Param, Msg[_,_]](value : scala.Double) : Double[T, Cond, Param, Msg] = new Double[T, Cond, Param, Msg](value)
  }

  final class String[T, Cond[_,_], Param, Msg[_,_]] private(val value : java.lang.String) extends AnyVal with CheckedAny[java.lang.String, T] with TwoFaceAny.String[T] {
    @inline def getValue : java.lang.String = value
  }
  object String extends CheckedAny.Builder[String, java.lang.String] {
    def create[T, Cond[_,_], Param, Msg[_,_]](value : java.lang.String) : String[T, Cond, Param, Msg] = new String[T, Cond, Param, Msg](value)
  }

  final class Boolean[T, Cond[_,_], Param, Msg[_,_]] private(val value : scala.Boolean) extends AnyVal with CheckedAny[scala.Boolean, T] with TwoFaceAny.Boolean[T] {
    @inline def getValue : scala.Boolean = value
  }
  object Boolean extends CheckedAny.Builder[Boolean, scala.Boolean] {
    def create[T, Cond[_,_], Param, Msg[_,_]](value : scala.Boolean) : Boolean[T, Cond, Param, Msg] = new Boolean[T, Cond, Param, Msg](value)
  }

  trait Runtime[TFace, ParamFace, Cond[_,_], Msg[_,_]] {
    def cond(t : TFace, p : Option[ParamFace]) : scala.Boolean
    def msg(t : TFace, p : Option[ParamFace]) : java.lang.String
  }
}
