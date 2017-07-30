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
  object One { //One Parameter Shell
    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait Char[FuncApply, FuncArgs, Arg1, Arg1Wide] {
      type Out <: scala.Char
      def apply(arg1 : Arg1Wide) : TwoFaceAny.Char[Out]
    }
    @bundle
    object Char {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide]:
      Char[FuncApply, FuncArgs, Arg1, Arg1Wide] =
      macro Macro.shell1[FuncApply, FuncArgs, Arg1, Arg1Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell1[FuncApply, FuncArgs, Arg1, Arg1Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide]) : c.Tree = TwoFaceShellMaterializer[
          Char[FuncApply, FuncArgs, Arg1, Arg1Wide]].shell1()
      }
    }

  }
  object Two { //Two Parameters Shell
    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait Char[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out <: scala.Char
      def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TwoFaceAny.Char[Out]
    }
    @bundle
    object Char {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]:
      Char[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] =
      macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
         arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
          Char[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
      }
    }

    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait Int[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out <: scala.Int
      def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TwoFaceAny.Int[Out]
    }
    @bundle
    object Int {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]:
      Int[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] =
      macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
         arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
          Int[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
      }
    }

    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait Long[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out <: scala.Long
      def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TwoFaceAny.Long[Out]
    }
    @bundle
    object Long {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]:
      Long[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] =
      macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
         arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
          Long[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
      }
    }

    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait Float[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out <: scala.Float
      def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TwoFaceAny.Float[Out]
    }
    @bundle
    object Float {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]:
      Float[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] =
      macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
         arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
          Float[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
      }
    }

    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait Double[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out <: scala.Double
      def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TwoFaceAny.Double[Out]
    }
    @bundle
    object Double {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]:
      Double[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] =
      macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
         arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
          Double[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
      }
    }

    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait String[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out <: java.lang.String
      def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TwoFaceAny.String[Out]
    }
    @bundle
    object String {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]:
      String[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] =
      macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
         arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
          String[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
      }
    }

    @scala.annotation.implicitNotFound("Unable to create shell for TwoFace")
    trait Boolean[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      type Out <: scala.Boolean
      def apply(arg1 : Arg1Wide, arg2 : Arg2Wide) : TwoFaceAny.Boolean[Out]
    }
    @bundle
    object Boolean {
      implicit def ev[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]:
      Boolean[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide] =
      macro Macro.shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]

      final class Macro(val c: whitebox.Context) extends GeneralMacros {
        def shell2[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]
        (implicit funcApply : c.WeakTypeTag[FuncApply], func : c.WeakTypeTag[FuncArgs],
         arg1 : c.WeakTypeTag[Arg1], arg1Wide : c.WeakTypeTag[Arg1Wide], arg2 : c.WeakTypeTag[Arg2],
         arg2Wide : c.WeakTypeTag[Arg2Wide]) : c.Tree = TwoFaceShellMaterializer[
          Boolean[FuncApply, FuncArgs, Arg1, Arg1Wide, Arg2, Arg2Wide]].shell2()
      }
    }

  }
}
