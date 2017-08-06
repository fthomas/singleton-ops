//package singleton.twoface.impl
//
//import macrocompat.bundle
//import singleton.ops._
//import singleton.ops.impl._
//import singleton.twoface.TwoFace
//
//import scala.reflect.macros.whitebox
//
//trait Checked1Param[Chk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace, T] extends Any with TwoFaceAny[Face] {
//  def unsafeCheck(p : ParamFace)
//  (implicit
//   cond : TwoFace.Boolean.Shell2[Cond, Face, Face, ParamFace, ParamFace],
//   msg : TwoFace.String.Shell2[Msg, Face, Face, ParamFace, ParamFace],
//   req : TwoFace.Boolean.Shell2[RequireMsg, Cond[Face, ParamFace], std.Boolean, Msg[Face, ParamFace], std.String]
//  ) = {
//    req(cond(getValue, p).getValue, msg(getValue, p).getValue)
//    this
//  }
//}
//
//object Checked1Param {
//  trait Builder[PrimChk[_,_], SecChk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace] {
//    type Shell[T, Param] <: Checked1ParamShell[PrimChk, Face, T, Param]
//
//    ////////////////////////////////////////////////////////////////////////////////////////
//    // Manual invocations (usually used to for testing)
//    ////////////////////////////////////////////////////////////////////////////////////////
//    def apply[T, Param](implicit value : AcceptNonLiteral[Id[T]], param : AcceptNonLiteral[Id[Param]])
//    : PrimChk[value.Out, param.Out] = macro Builder.Macro.fromOpApply[PrimChk[_,_], SecChk[_,_], Cond[_,_], Msg[_,_]]
//
//    def apply[T <: Face, Param <: ParamFace](value : T)(param : Param)
//    : PrimChk[T, Param] = macro Builder.Macro.fromNumValue[PrimChk[_,_], SecChk[_,_], Cond[_,_], Msg[_,_]]
//
//    def apply[T0 <: Face, Param <: ParamFace](value : TwoFaceAny[Face]{type T = T0})(param : TwoFaceAny[ParamFace]{type T = Param})
//    : PrimChk[T0, Param] = macro Builder.Macro.fromTF[PrimChk[_,_], SecChk[_,_], Cond[_,_], Msg[_,_]]
//    ////////////////////////////////////////////////////////////////////////////////////////
//
//    ////////////////////////////////////////////////////////////////////////////////////////
//    // Implicit Conversions
//    ////////////////////////////////////////////////////////////////////////////////////////
//    implicit def ev[T, Param](implicit value : AcceptNonLiteral[Id[T]], param : AcceptNonLiteral[Id[Param]])
//    : PrimChk[T, Param] = macro Builder.Macro.fromOpImpl[T, Param, PrimChk[_,_], SecChk[_,_], Cond[_,_], Msg[_,_]]
//
//    implicit def fromNum[T <: Face, Param <: ParamFace](value : T)(implicit param : AcceptNonLiteral[Id[Param]])
//    : PrimChk[T, Param] = macro Builder.Macro.fromNumValue[PrimChk[_,_], SecChk[_,_], Cond[_,_], Msg[_,_]]
//
//    implicit def fromTF[T0 <: Face, Param <: ParamFace](value : TwoFaceAny[Face]{type T = T0})(implicit param : TwoFaceAny[ParamFace]{type T = Param})
//    : PrimChk[T0, Param] = macro Builder.Macro.fromTF[PrimChk[_,_], SecChk[_,_], Cond[_,_], Msg[_,_]]
//    ////////////////////////////////////////////////////////////////////////////////////////
//  }
//
//  @bundle
//  object Builder {
//    final class Macro(val c: whitebox.Context) extends GeneralMacros {
//      def fromOpApply[PrimChk, SecChk, Cond, Msg](value : c.Tree, param : c.Tree)(
//        implicit
//        primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
//      ): c.Tree = Checked1ParamMaterializer[PrimChk, SecChk, Cond, Msg].fromOpApply(value, param)
//
//      def fromOpImpl[T, Param, PrimChk, SecChk, Cond, Msg](value : c.Tree)(param : c.Tree)(
//        implicit
//        t : c.WeakTypeTag[T], p : c.WeakTypeTag[T], primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
//      ): c.Tree = Checked1ParamMaterializer[PrimChk, SecChk, Cond, Msg].fromOpImpl(value, c.weakTypeOf[T], param, c.weakTypeOf[Param])
//
//      def fromNumValue[PrimChk, SecChk, Cond, Msg](value : c.Tree)(param : c.Tree)(
//        implicit
//        primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
//      ): c.Tree = Checked1ParamMaterializer[PrimChk, SecChk, Cond, Msg].fromNumValue(value, param)
//
//      def fromTF[PrimChk, SecChk, Cond, Msg](value : c.Tree)(param : c.Tree)(
//        implicit
//        primChk : c.WeakTypeTag[PrimChk], secChk : c.WeakTypeTag[SecChk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg]
//      ): c.Tree = Checked1ParamMaterializer[PrimChk, SecChk, Cond, Msg].fromTF(value, param)
//    }
//  }
//}
//
//
//trait Checked1ParamShell[Chk[_,_], Face, T, Param] {
//  def apply(value : Face) : Chk[_,_]
//}
//
//object Checked1ParamShell {
//  trait Builder[ChkShl[_,_], Chk[_,_], Cond[_,_], Msg[_,_], Face, ParamFace] {
//    type MsgHelper[T, Param] = ITE[IsNonLiteral[Msg[T, Param]], SomethingBadHappened, Msg[T, Param]]
//    type CondHelper[T, Param] =
//      RequireMsgSym[ITE[IsNonLiteral[Cond[T, Param]], True, Cond[T, Param]], MsgHelper[T, Param], ChkShl[_,_]]
//    def create[T, Param] : ChkShl[T, Param]
//
//    implicit def impl[T, Param]
//    (implicit vc : CondHelper[T, Param]) :
//    ChkShl[T, Param] = create[T, Param]
//  }
//}
