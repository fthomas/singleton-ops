package singleton.twoface
import impl._
//import singleton.ops._

object Checked {
  trait Char[T, Param] extends Any with CheckedAny[scala.Char, T] with TwoFaceAny.Char[T]
  trait Int[T, Param] extends Any with CheckedAny[scala.Int, T] with TwoFaceAny.Int[T]
  trait Long[T, Param] extends Any with CheckedAny[scala.Long, T] with TwoFaceAny.Long[T]
  trait Float[T, Param] extends Any with CheckedAny[scala.Float, T] with TwoFaceAny.Float[T]
  trait Double[T, Param] extends Any with CheckedAny[scala.Double, T] with TwoFaceAny.Double[T]
  trait String[T, Param] extends Any with CheckedAny[java.lang.String, T] with TwoFaceAny.String[T]
  trait Boolean[T, Param] extends Any with CheckedAny[scala.Boolean, T] with TwoFaceAny.Boolean[T]
}
