package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops.impl.GeneralMacros
import scala.reflect.macros.whitebox


////////////////////////////////////////////////////////////////////////////////
// The code duplication in `Shell` can indeed be reduced.
// However, the duplication was favored because it had caused less 'red' in
// IntelliJ, when the `apply` in the `Shell` traits has a concrete return type.
// Example red code without concrete return type:
// def testme[L <: Int,R <: Int](l : TwoFace.Int[L], r : TwoFace.Int[R])
//     (implicit tfs : TwoFace.Int.Shell2[+,L,Int,R,Int]) = tfs(l,r)
// val tmres = testme(TwoFace.Int(2),testme(TwoFace.Int(2),TwoFace.Int(2)))
////////////////////////////////////////////////////////////////////////////////
object Shell {
  @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
  trait One[FuncApply, FuncArgs, Arg1, Arg1Wide] {
    type Out
    type TF[T]
    def apply(arg1 : Arg1Wide) : TF[Out]
  }

  object One { //One Parameter Shell
    type Aux[FuncApply, FuncArgs, Arg1, Arg1Wide, Ret_Out, Ret_TF[T]] =
      One[FuncApply, FuncArgs, Arg1, Arg1Wide] {
        type Out = Ret_Out
        type TF[T] = Ret_TF[T]
      }
    implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Ret_Out, Ret_TF[T]]:
    Aux[FuncApply, FuncArgs, Arg1, Arg1Wide, Ret_Out, Ret_TF] =
    macro Macro.shell1[FuncApply, FuncArgs, Arg1, Arg1Wide]

    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def shell1[FuncApply, FuncArgs, Arg1, Arg1Wide]
      (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
       arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide]) : c.Tree = TwoFaceShellMaterializer[
        One[FuncApply, FuncArgs, Arg1, Arg1Wide]].shell1()
    }
  }


  @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
  trait Two[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
    type Out
    type TF[T]
    def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TF[Out]
  }

  @bundle
  object Two { //Two Parameters Shell
    type Aux[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Ret_Out, Ret_TF[T]] =
    Two[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out = Ret_Out
      type TF[T] = Ret_TF[T]
    }
    implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Ret_Out, Ret_TF[T]]:
    Aux[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Ret_Out, Ret_TF] =
    macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
      (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
       arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
       arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
        Two[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
    }
  }


  @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
  trait Three[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide] {
    type Out
    type TF[T]
    def apply(arg1 : Arg1Wide, arg2 : Arg2Wide, arg3 : Arg3Wide) : TF[Out]
  }

  @bundle
  object Three { //Two Parameters Shell
  type Aux[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide, Ret_Out, Ret_TF[T]] =
    Three[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide] {
      type Out = Ret_Out
      type TF[T] = Ret_TF[T]
    }
    implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide, Ret_Out, Ret_TF[T]]:
    Aux[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide, Ret_Out, Ret_TF] =
    macro Macro.shell3[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]

    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def shell3[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]
      (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
       arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
       arg2Wide : c.WeakTypeTag[Arg2Wide], arg3 : c.WeakTypeTag[Arg3],
       arg3Wide : c.WeakTypeTag[Arg3Wide]) : c.Tree = TwoFaceShellMaterializer[
        Three[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]].shell3()
    }
  }
}
