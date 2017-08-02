package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import scala.reflect.macros.whitebox

trait Checked0Param[Cond[_], Msg[_], Face, T] extends Any with TwoFaceAny[Face, T] {
  def unsafeCheck()
  (implicit
   cond : TwoFaceAny.Boolean.Shell1[Cond, Face, Face],
   msg : TwoFaceAny.String.Shell1[Msg, Face, Face],
   req : TwoFaceAny.Boolean.Shell2[RequireMsg, Cond[Face], scala.Boolean, Msg[Face], java.lang.String]
  ) : this.type = {
    req(cond(getValue), msg(getValue))
    this
  }
}

object Checked0Param {
  trait Builder[Chk[T], Cond[_], Msg[_], Face] {
    type Shell[T] <: Checked0ParamShell[Chk, Face, T]
    type CondHelper[T] = ITE[IsNonLiteral[Cond[T]], True, Cond[T]]
    type MsgHelper[T] = ITE[IsNonLiteral[Msg[T]], SomethingBadHappened, Msg[T]]

    implicit def impl[T]
    (implicit vc : CondHelper[T], vm : MsgHelper[T]) :
    Chk[T] = macro Builder.Macro.impl[T, Chk[T]]

    implicit def safe[T <: Face with Singleton]
    (value : T)(implicit vc : CondHelper[T], vm : MsgHelper[T]) :
    Chk[T] = macro Builder.Macro.safe[T, Chk[T]]

    implicit def unsafe[Param](value : Face) :
    Chk[Face] = macro Builder.Macro.unsafe[Face, Chk[Face]]

    implicit def safeTF[T <: Face with Singleton]
    (value : TwoFaceAny[Face, T])(implicit vc : CondHelper[T], vm : MsgHelper[T]) :
    Chk[T] = macro Builder.Macro.safeTF[T, Chk[T]]

    implicit def unsafeTF[Param](value : TwoFaceAny[Face, Face]) :
    Chk[Face] = macro Builder.Macro.unsafeTF[Face, Chk[Face]]
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def impl[T, Chk](vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].impl(0, vc, vm)

      def safe[T, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].impl(0, vc, vm)

      def safeTF[T, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].impl(0, vc, vm)

      def unsafe[T, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].unsafe(0, value)

      def unsafeTF[T, Chk](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
      ): c.Tree = CheckedImplMaterializer[T, T, Chk].unsafeTF(0, value)
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
