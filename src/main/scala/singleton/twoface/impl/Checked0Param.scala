package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import singleton.twoface.TwoFace

import scala.reflect.macros.whitebox

trait Checked0Param[Chk[_], Cond[_], Msg[_], Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck()
  (implicit
   cond : TwoFace.Boolean.Shell1[Cond, Face, Face],
   msg : TwoFace.String.Shell1[Msg, Face, Face],
   req : TwoFace.Boolean.Shell2[RequireMsg, Cond[Face], std.Boolean, Msg[Face], std.String]
  ) : Chk[T] = {
    req(cond(getValue).getValue, msg(getValue).getValue)
    this.asInstanceOf[Chk[T]]
  }
}

object Checked0Param {
  trait Builder[Chk[T], Cond[_], Msg[_], Face] {
    type Shell[T] <: Checked0ParamShell[Chk, Face, T]

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

    implicit def fromNum[T <: Face, Out <: T](value : T)
    : Chk[Out] = macro Builder.Macro.fromNumValue[Chk[_], Cond[_], Msg[_]]

    implicit def fromTF[T <: Face, Out <: T](value : TwoFaceAny[Face, T])
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


trait Checked0ParamShell[Chk[_], Face, T] {
  def apply(value : Face) : Chk[_]
}

object Checked0ParamShell {
  trait Builder[ChkShl[_], Chk[_], Cond[_], Msg[_], Face] {
    type MsgHelper[T] = ITE[IsNonLiteral[Msg[T]], SomethingBadHappened, Msg[T]]
    type CondHelper[T] = RequireMsgSym[ITE[IsNonLiteral[Cond[T]], True, Cond[T]], MsgHelper[T],ChkShl[_]]
    def create[T] : ChkShl[T]

    implicit def impl[T]
    (implicit vc : CondHelper[T]) :
    ChkShl[T] = create[T]
  }
}
