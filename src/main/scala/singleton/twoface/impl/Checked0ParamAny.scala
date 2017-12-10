package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._

import scala.reflect.macros.whitebox

trait Checked0ParamAny[Chk[Cond0[_], Msg0[_], T0], Cond[_], Msg[_], Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck()(implicit shl : CheckedShell1[Cond, Msg, Chk[Cond, Msg, _], Face, Face]) : Chk[Cond, Msg, T] = {
    shl.unsafeCheck(getValue)
    this.asInstanceOf[Chk[Cond, Msg, T]]
  }
}

object Checked0ParamAny {
  trait Builder[Chk[Cond0[_], Msg0[_], T0], Face] {
    trait Alias {
      type Cond[T]
      type Msg[T]
      type Checked[T] = Chk[Cond, Msg, T]
      type CheckedShell[T] = CheckedShellSym[CheckedShellSym[_,_], T]
      type CheckedShellSym[Sym, T] = CheckedShell1[Cond, Msg, Sym, T, Face]
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    // Generic Implicit Conversions
    // Not used generically, but repeated to avoid IntelliJ issue
    // https://youtrack.jetbrains.com/issue/SCL-13089
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[Cond[_], Msg[_], T](implicit value : AcceptNonLiteral[Id[T]])
    : Chk[Cond, Msg, T] = macro Builder.Macro.fromOpImpl[T, Chk[Cond, Msg, _], Cond[_], Msg[_]]

    implicit def fromNum[Cond[_], Msg[_], T >: Face, Out <: T](value : T)
    : Chk[Cond, Msg, Out] = macro Builder.Macro.fromNumValue[Chk[Cond, Msg, _], Cond[_], Msg[_]]

    implicit def fromTF[Cond[_], Msg[_], T >: Face, Out <: T](value : TwoFaceAny[Face, T])
    : Chk[Cond, Msg, Out] = macro Builder.Macro.fromTF[Chk[Cond, Msg, _], Cond[_], Msg[_]]
    ////////////////////////////////////////////////////////////////////////////////////////
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromOpApply[Chk, Cond, Msg](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg].fromOpApply(value)

      def fromOpImpl[T, Chk, Cond, Msg](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg].fromOpImpl(value, c.weakTypeOf[T])

      def fromNumValue[Chk, Cond, Msg](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg].fromNumValue(value)

      def fromTF[Chk, Cond, Msg](value : c.Tree)(
        implicit
        chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = Checked0ParamMaterializer[Chk, Cond, Msg].fromTF(value)
    }
  }

  final class Int[Cond[_], Msg[_], T](val value : std.Int) extends
    Checked0ParamAny[Int, Cond, Msg, std.Int, T] with TwoFaceAny.Int[T] {
    @inline def getValue : std.Int = value
  }
  object Int extends Builder[Int, std.Int]

  final class Boolean[Cond[_], Msg[_], T](val value : std.Boolean) extends
    Checked0ParamAny[Boolean, Cond, Msg, std.Boolean, T] with TwoFaceAny.Boolean[T] {
    @inline def getValue : std.Boolean = value
  }
  object Boolean extends Builder[Boolean, std.Boolean]
}

