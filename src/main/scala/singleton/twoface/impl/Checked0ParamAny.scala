package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._

import scala.reflect.macros.whitebox

trait Checked0ParamAny[Chk[_], Cond[_], Msg[_], Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck()(implicit shl : CheckedShell1[Cond, Msg, Chk[_], Face, Face]) : Chk[T] = {
    shl.unsafeCheck(getValue)
    this.asInstanceOf[Chk[T]]
  }
}

object Checked0ParamAny {
  trait Builder[Chk[T], Cond[_], Msg[_], Face] {
    type Shell[T] = ShellSym[ShellSym[_,_], T]
    trait ShellSym[Sym, T] extends CheckedShell1[Cond, Msg, Sym, T, Face]
    object ShellSym extends CheckedShell1Builder[ShellSym, Cond, Msg, Face] {
      def create[Sym, T](_unsafeCheck: Face => Unit) : ShellSym[Sym, T] = new ShellSym[Sym, T] {
        def unsafeCheck(arg1: => Face): Unit = _unsafeCheck(arg1)
      }
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    // Manual invocations (usually used to for testing)
    ////////////////////////////////////////////////////////////////////////////////////////
    def apply[T](implicit value : AcceptNonLiteral[Id[T]])
    : Chk[value.Out] = macro Builder.Macro.fromOpApply[Chk[_], Cond[_], Msg[_]]

    def apply[T <: Face, Out <: T](value : T)
    : Chk[Out] = macro Builder.Macro.fromNumValue[Chk[_], Cond[_], Msg[_]]

    def apply[T <: Face, Out <: T](value : TwoFaceAny[Face, T])
    : Chk[Out] = macro Builder.Macro.fromTF[Chk[_], Cond[_], Msg[_]]
    ////////////////////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////////////////////
    // Implicit Conversions
    ////////////////////////////////////////////////////////////////////////////////////////
    implicit def ev[T](implicit value : AcceptNonLiteral[Id[T]])
    : Chk[T] = macro Builder.Macro.fromOpImpl[T, Chk[_], Cond[_], Msg[_]]

    implicit def fromNum[T >: Face, Out <: T](value : T)
    : Chk[Out] = macro Builder.Macro.fromNumValue[Chk[_], Cond[_], Msg[_]]

    implicit def fromTF[T >: Face, Out <: T](value : TwoFaceAny[Face, T])
    : Chk[Out] = macro Builder.Macro.fromTF[Chk[_], Cond[_], Msg[_]]
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
}

