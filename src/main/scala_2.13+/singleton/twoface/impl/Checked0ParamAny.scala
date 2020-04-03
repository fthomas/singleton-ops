package singleton.twoface.impl

import singleton.ops._
import singleton.ops.impl._

import scala.reflect.macros.whitebox

trait Checked0ParamAny[Chk[Cond0[_], Msg0[_], T0], Cond[_], Msg[_], Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck()(implicit shl : CheckedShell1[Cond, Msg, NoSym, Face, Face]) : Chk[Cond, Msg, T] = {
    shl.unsafeCheck(getValue)
    this.asInstanceOf[Chk[Cond, Msg, T]]
  }
}

object Checked0ParamAny {
  trait LP[Chk[Cond0[_], Msg0[_], T0], Face] {
    def create[Cond[_], Msg[_], T](value : Face) : Chk[Cond, Msg, T]

    //This is a hack to force Scalac to actually display the constructed error message in all cases
    //From some weird reason only direct macro calls displays the error in a case of an implicit conversion
    implicit def requireMsg[Cond[_], Msg[_], T >: Face, Out <: T](value : T)
    : Chk[Cond, Msg, Out] = macro Builder.Macro.fromNumValue[Chk[Cond,Msg,_], Cond[_], Msg[_], T]

    //from non-singleton values
    implicit def fromNumNonSing[Cond[_], Msg[_]](value : Face)(
      implicit req : ITE[IsNonLiteral[GetArg0], true, RequireMsg[Cond[GetArg0], Msg[GetArg0]]], di : DummyImplicit
    ) : Chk[Cond, Msg, Face] = create[Cond, Msg, Face](value)
  }

  trait Builder[Chk[Cond0[_], Msg0[_], T0], Face] extends LP[Chk, Face] {
    trait Alias {
      type Cond[T]
      type Msg[T]
      final type Checked[T] = Chk[Cond, Msg, T]
      final type CheckedShell[T] = CheckedShellSym[NoSym, T]
      final type CheckedShellSym[Sym, T] = CheckedShell1[Cond, Msg, Sym, T, Face]
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    // Generic Implicit Conversions
    // Currently triggers good-code-red IntelliJ issue
    // https://youtrack.jetbrains.com/issue/SCL-13089
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[Cond[_], Msg[_], T](implicit req : RequireMsg[Cond[T], Msg[T]], value : AcceptNonLiteral[Id[T]])
    : Chk[Cond, Msg, T] = create[Cond, Msg, T](value.valueWide.asInstanceOf[Face])

    implicit def fromNumSing[Cond[_], Msg[_], T <: Face with Singleton](value : T)(
      implicit req : RequireMsg[Cond[T], Msg[T]]
    ) : Chk[Cond, Msg, T] = create[Cond, Msg, T](value)

    implicit def fromTF[Cond[_], Msg[_], T](tfValue : TwoFaceAny[Face, T])(
      implicit req : RequireMsg[IsNonLiteral[T] || Cond[T], Msg[T]], di : DummyImplicit
    ) : Chk[Cond, Msg, T] = create[Cond, Msg, T](tfValue.getValue)

    implicit def argCast[Cond[_], Msg[_], F, T](c : Chk[Cond, Msg, F])(
      implicit eq : OpContainer.Eq[F, T, Face]
    ) : Chk[Cond, Msg, T] = c.asInstanceOf[Chk[Cond, Msg, T]]
    ////////////////////////////////////////////////////////////////////////////////////////
  }

  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromNumValue[Chk, Cond, Msg, T](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg, T].fromNumValue(value)
    }
  }

  final class Char[Cond[_], Msg[_], T](val value : std.Char) extends
    Checked0ParamAny[Char, Cond, Msg, std.Char, T] with TwoFaceAny.Char[T] {
    @inline def getValue : std.Char = value
  }
  object Char extends Builder[Char, std.Char] {
    def create[Cond[_], Msg[_], T](value: std.Char): Char[Cond, Msg, T] = new Char[Cond, Msg, T](value)
  }

  @scala.annotation.implicitNotFound("haha")
  final class Int[Cond[_], Msg[_], T](val value : std.Int) extends
    Checked0ParamAny[Int, Cond, Msg, std.Int, T] with TwoFaceAny.Int[T] {
    @inline def getValue : std.Int = value
  }
  object Int extends Builder[Int, std.Int] {
    def create[Cond[_], Msg[_], T](value: std.Int): Int[Cond, Msg, T] = new Int[Cond, Msg, T](value)
  }

  final class Long[Cond[_], Msg[_], T](val value : std.Long) extends
    Checked0ParamAny[Long, Cond, Msg, std.Long, T] with TwoFaceAny.Long[T] {
    @inline def getValue : std.Long = value
  }
  object Long extends Builder[Long, std.Long] {
    def create[Cond[_], Msg[_], T](value: std.Long): Long[Cond, Msg, T] = new Long[Cond, Msg, T](value)
  }

  final class Float[Cond[_], Msg[_], T](val value : std.Float) extends
    Checked0ParamAny[Float, Cond, Msg, std.Float, T] with TwoFaceAny.Float[T] {
    @inline def getValue : std.Float = value
  }
  object Float extends Builder[Float, std.Float] {
    def create[Cond[_], Msg[_], T](value: std.Float): Float[Cond, Msg, T] = new Float[Cond, Msg, T](value)
  }

  final class Double[Cond[_], Msg[_], T](val value : std.Double) extends
    Checked0ParamAny[Double, Cond, Msg, std.Double, T] with TwoFaceAny.Double[T] {
    @inline def getValue : std.Double = value
  }
  object Double extends Builder[Double, std.Double] {
    def create[Cond[_], Msg[_], T](value: std.Double): Double[Cond, Msg, T] = new Double[Cond, Msg, T](value)
  }

  final class String[Cond[_], Msg[_], T](val value : std.String) extends
    Checked0ParamAny[String, Cond, Msg, std.String, T] with TwoFaceAny.String[T] {
    @inline def getValue : std.String = value
  }
  object String extends Builder[String, std.String] {
    def create[Cond[_], Msg[_], T](value: std.String): String[Cond, Msg, T] = new String[Cond, Msg, T](value)
  }

  final class Boolean[Cond[_], Msg[_], T](val value : std.Boolean) extends
    Checked0ParamAny[Boolean, Cond, Msg, std.Boolean, T] with TwoFaceAny.Boolean[T] {
    @inline def getValue : std.Boolean = value
  }
  object Boolean extends Builder[Boolean, std.Boolean] {
    def create[Cond[_], Msg[_], T](value: std.Boolean): Boolean[Cond, Msg, T] = new Boolean[Cond, Msg, T](value)
  }
}

