package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._

import scala.reflect.macros.whitebox

trait Checked0ParamAny[Chk[Cond0[_], Msg0[_], T0], Cond[_], Msg[_], Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck()(implicit shl : CheckedShell1[Cond, Msg, Chk[Cond,Msg,_], Face, Face]) : Chk[Cond, Msg, T] = {
    shl.unsafeCheck(getValue)
    this.asInstanceOf[Chk[Cond, Msg, T]]
  }
}

object Checked0ParamAny {
  trait Builder[Chk[Cond0[_], Msg0[_], T0], Face] {
    trait Alias {
      type Cond[T]
      type Msg[T]
      final type Checked[T] = Chk[Cond, Msg, T]
      final type CheckedShell[T] = CheckedShellSym[CheckedShellSym[_,_], T]
      final type CheckedShellSym[Sym, T] = CheckedShell1[Cond, Msg, Sym, T, Face]
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    // Generic Implicit Conversions
    // Currently triggers good-code-red IntelliJ issue
    // https://youtrack.jetbrains.com/issue/SCL-13089
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[Cond[_], Msg[_], T](implicit value : AcceptNonLiteral[Id[T]])
    : Chk[Cond, Msg, T] = macro Builder.Macro.fromOpImpl[Chk[Cond,Msg,_], Cond[_], Msg[_], T]

    implicit def fromNum[Cond[_], Msg[_], T >: Face, Out <: T](value : T)
    : Chk[Cond, Msg, Out] = macro Builder.Macro.fromNumValue[Chk[Cond,Msg,_], Cond[_], Msg[_], T]

    implicit def fromTF[Cond[_], Msg[_], T >: Face, Out <: T](value : TwoFaceAny[Face, T])
    : Chk[Cond, Msg, Out] = macro Builder.Macro.fromTF[Chk[Cond,Msg,_], Cond[_], Msg[_], T]

    implicit def widen[Cond[_], Msg[_], T](value : Chk[Cond, Msg, T])
    : Chk[Cond, Msg, Face] = macro Builder.Macro.widen[Chk[Cond,Msg,_], Cond[_], Msg[_], Face]
    ////////////////////////////////////////////////////////////////////////////////////////
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromOpImpl[Chk, Cond, Msg, T](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg, T].fromOpImpl(value)

      def fromNumValue[Chk, Cond, Msg, T](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg, T].fromNumValue(value)

      def fromTF[Chk, Cond, Msg, T](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg, T].fromTF(value)

      def widen[Chk, Cond, Msg, T](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg, T].widen(value)
    }
  }

  final class Char[Cond[_], Msg[_], T](val value : std.Char) extends
    Checked0ParamAny[Char, Cond, Msg, std.Char, T] with TwoFaceAny.Char[T] {
    @inline def getValue : std.Char = value
  }
  object Char extends Builder[Char, std.Char]

  final class Int[Cond[_], Msg[_], T](val value : std.Int) extends
    Checked0ParamAny[Int, Cond, Msg, std.Int, T] with TwoFaceAny.Int[T] {
    @inline def getValue : std.Int = value
  }
  object Int extends Builder[Int, std.Int]

  final class Long[Cond[_], Msg[_], T](val value : std.Long) extends
    Checked0ParamAny[Long, Cond, Msg, std.Long, T] with TwoFaceAny.Long[T] {
    @inline def getValue : std.Long = value
  }
  object Long extends Builder[Long, std.Long]

  final class Float[Cond[_], Msg[_], T](val value : std.Float) extends
    Checked0ParamAny[Float, Cond, Msg, std.Float, T] with TwoFaceAny.Float[T] {
    @inline def getValue : std.Float = value
  }
  object Float extends Builder[Float, std.Float]

  final class Double[Cond[_], Msg[_], T](val value : std.Double) extends
    Checked0ParamAny[Double, Cond, Msg, std.Double, T] with TwoFaceAny.Double[T] {
    @inline def getValue : std.Double = value
  }
  object Double extends Builder[Double, std.Double]

  final class String[Cond[_], Msg[_], T](val value : std.String) extends
    Checked0ParamAny[String, Cond, Msg, std.String, T] with TwoFaceAny.String[T] {
    @inline def getValue : std.String = value
  }
  object String extends Builder[String, std.String]

  final class Boolean[Cond[_], Msg[_], T](val value : std.Boolean) extends
    Checked0ParamAny[Boolean, Cond, Msg, std.Boolean, T] with TwoFaceAny.Boolean[T] {
    @inline def getValue : std.Boolean = value
  }
  object Boolean extends Builder[Boolean, std.Boolean]
}

