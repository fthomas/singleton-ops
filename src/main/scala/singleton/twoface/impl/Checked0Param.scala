package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl._
import singleton.twoface.TwoFace

import scala.reflect.macros.whitebox

trait Checked0Param[Cond[_], Msg[_], Face] extends Any with TwoFaceAny[Face] {
  def unsafeCheck()
  (implicit
   cond : TwoFace.Boolean.Shell1[Cond, Face, Face],
   msg : TwoFace.String.Shell1[Msg, Face, Face],
   req : TwoFace.Boolean.Shell2[RequireMsg, Cond[Face], std.Boolean, Msg[Face], std.String]
  ) = {
    req(cond(getValue).getValue, msg(getValue).getValue)
    this
  }
}

object Checked0Param {
  trait Builder[PrimChk[T], SecChk[T], Cond[_], Msg[_], Face] {
    type Shell[T] <: Checked0ParamShell[PrimChk, Face, T]

    def apply[T](implicit value : AcceptNonLiteral[Id[T]])
    : PrimChk[value.Out] = macro Builder.Macro.fromOpApply[PrimChk[_], SecChk[_], Cond[_], Msg[_]]

//    implicit def ev[T](implicit value : AcceptNonLiteral[Id[T]])
//    : PrimChk[T] = macro Builder.Macro.fromOpImpl[T, PrimChk[_], SecChk[_], Cond[_], Msg[_]]

    implicit def apply[T <: Face](value : T)
    : PrimChk[T] = macro Builder.Macro.fromNumValue[PrimChk[_], SecChk[_], Cond[_], Msg[_]]

    implicit def apply[T0 <: Face](value : TwoFaceAny[Face]{type T <: T0}, di: DummyImplicit)
    : PrimChk[T0] = macro Builder.Macro.fromTF[PrimChk[_], SecChk[_], Cond[_], Msg[_]]
//    implicit def impl[T]
//    (implicit vc : CondHelper[T], vm : MsgHelper[T]) :
//    PrimChk[T] = macro Builder.Macro.impl[T, PrimChk[T]]
//
//    implicit def safe[T <: Face with Singleton]
//    (value : T)(implicit vc : CondHelper[T], vm : MsgHelper[T]) :
//    PrimChk[T] = macro Builder.Macro.safe[T, PrimChk[T]]
//
//    implicit def unsafe[Param](value : Face) :
//    PrimChk[Face] = macro Builder.Macro.unsafe[Face, PrimChk[Face]]
//
//    implicit def safeTF[T0 <: Face with Singleton]
//    (value : TwoFaceAny[Face]{type T})(implicit vc : CondHelper[T0], vm : MsgHelper[T0]) :
//    PrimChk[T0] = macro Builder.Macro.safeTF[T0, PrimChk[T0]]
//
//    implicit def unsafeTF[Param](value : TwoFaceAny[Face, Face]) :
//    PrimChk[Face] = macro Builder.Macro.unsafeTF[Face, PrimChk[Face]]
  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromOpApply[PrimChk, SecChk, Cond, Msg](value : c.Tree)(
        implicit
        primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = CheckedImplMaterializer[PrimChk, SecChk, Cond, Msg].fromOpApply(0, value)

      def fromOpImpl[T, PrimChk, SecChk, Cond, Msg](value : c.Tree)(
        implicit
        t : c.WeakTypeTag[T], primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = CheckedImplMaterializer[PrimChk, SecChk, Cond, Msg].fromOpImpl(0, value, c.weakTypeOf[T])

      def fromNumValue[PrimChk, SecChk, Cond, Msg](value : c.Tree)(
        implicit
        primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = CheckedImplMaterializer[PrimChk, SecChk, Cond, Msg].fromNumValue(0, value)

      def fromTF[PrimChk, SecChk, Cond, Msg](value : c.Tree, di : c.Tree)(
        implicit
        primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
      ): c.Tree = CheckedImplMaterializer[PrimChk, SecChk, Cond, Msg].fromTF(0, value)
//      def impl[T, Chk](vc : c.Tree, vm : c.Tree)(
//        implicit
//        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
//      ): c.Tree = CheckedImplMaterializer[T, T, Chk].fromNumValue(0, vc, vm)
//
//      def safeTF[T, Chk](value : c.Tree)(vc : c.Tree, vm : c.Tree)(
//        implicit
//        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
//      ): c.Tree = CheckedImplMaterializer[T, T, Chk].fromNumValue(0, vc, vm)
//
//      def unsafe[T, Chk](value : c.Tree)(
//        implicit
//        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
//      ): c.Tree = CheckedImplMaterializer[T, T, Chk].unsafe(0, value)
//
//      def unsafeTF[T, Chk](value : c.Tree)(
//        implicit
//        t : c.WeakTypeTag[T], chk: c.WeakTypeTag[Chk]
//      ): c.Tree = CheckedImplMaterializer[T, T, Chk].unsafeTF(0, value)
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
