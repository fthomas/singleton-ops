package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl.GeneralMacros
import singleton.ops.impl.std

import scala.reflect.macros.whitebox

trait TwoFaceAny[Face, T] extends Any {
  type Out = T
  def isLiteral(implicit rt : RunTime[T]) : std.Boolean = !rt
  @inline def getValue : Face
  override def toString = getValue.toString
}

object TwoFaceAny {
  trait Builder[TF[T], Face, Shl1[_,_,_,_], Shl2[_,_,_,_,_,_], Shl3[_,_,_,_,_,_,_,_]] {
    //////////////////////////////////////////////////////////////////
    type Shell1[Func[_], Arg1, Arg1Wide] =
      Shl1[Func[Arg1],
           Func[Arg[W.`1`.T, Arg1, Arg1Wide]],
           Arg1, Arg1Wide]
    type Shell1Aux[Func[_], Arg1, Arg1Wide, RetOut] =
      Shell1[Func, Arg1, Arg1Wide]{type Out = RetOut}
    //////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////
    type Shell2[Func[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shl2[Func[Arg1, Arg2],
           Func[Arg[W.`1`.T, Arg1, Arg1Wide], Arg[W.`2`.T, Arg2, Arg2Wide]],
           Arg1, Arg1Wide, Arg2, Arg2Wide]
    type Shell2Aux[Func[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide, RetOut] =
      Shell2[Func, Arg1, Arg1Wide, Arg2, Arg2Wide]{type Out = RetOut}
    //////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////
    type Shell3[Func[_,_,_], Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide] =
      Shl3[Func[Arg1, Arg2, Arg3],
           Func[Arg[W.`1`.T, Arg1, Arg1Wide], Arg[W.`2`.T, Arg2, Arg2Wide], Arg[W.`3`.T, Arg3, Arg3Wide]],
           Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]
    type ShellAux[Func[_,_,_], Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide, RetOut] =
      Shell3[Func, Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]{type Out = RetOut}
    //////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////
    //Shell1 with a casting operation on the argument
    //////////////////////////////////////////////////////////////////
    type Shell1D[Func[_], Arg1, Arg1Wide] =
      Shl1[Func[ToDouble[Arg1]],
        Func[ToDouble[Arg[W.`1`.T, Arg1, Arg1Wide]]],
        Arg1, Arg1Wide]
    //////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////
    //Shell2 with a casting operation on both arguments
    //////////////////////////////////////////////////////////////////
    //Shell for Int
    protected[twoface] type ShellI[Func[A,B], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shl2[Func[ToInt[Arg1], ToInt[Arg2]],
        Func[ToInt[Arg[W.`1`.T, Arg1, Arg1Wide]], ToInt[Arg[W.`2`.T, Arg2, Arg2Wide]]],
        Arg1, Arg1Wide, Arg2, Arg2Wide]
    //Shell for Long
    protected[twoface] type ShellL[Func[A,B], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shl2[Func[ToLong[Arg1], ToLong[Arg2]],
        Func[ToLong[Arg[W.`1`.T, Arg1, Arg1Wide]], ToLong[Arg[W.`2`.T, Arg2, Arg2Wide]]],
        Arg1, Arg1Wide, Arg2, Arg2Wide]
    //Shell for Float
    protected[twoface] type ShellF[Func[A,B], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shl2[Func[ToFloat[Arg1], ToFloat[Arg2]],
        Func[ToFloat[Arg[W.`1`.T, Arg1, Arg1Wide]], ToFloat[Arg[W.`2`.T, Arg2, Arg2Wide]]],
        Arg1, Arg1Wide, Arg2, Arg2Wide]
    //Shell for Double
    protected[twoface] type ShellD[Func[A,B], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shl2[Func[ToDouble[Arg1], ToDouble[Arg2]],
        Func[ToDouble[Arg[W.`1`.T, Arg1, Arg1Wide]], ToDouble[Arg[W.`2`.T, Arg2, Arg2Wide]]],
        Arg1, Arg1Wide, Arg2, Arg2Wide]
    //////////////////////////////////////////////////////////////////

    def create[T](value : Face) : TF[T]

    //The implicit conversion from numeric to TwoFace could have been implemented generically, like the following.
    //However, since IntelliJ marks everything red, it was preferred to implement it specifically, in the meantime.
    //implicit def apply[T <: Face](value : T) : Lt[T] = macro Builder.Macro.fromNumValue

  }


  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromNumValue[TF](value : c.Tree)(implicit tfTag : c.WeakTypeTag[TF]) : c.Tree =
        TwoFaceMaterializer.fromNumValue(value, c.symbolOf[TF])
      def toNumValue[TF, T](tf : c.Tree)(implicit tTag : c.WeakTypeTag[T], tfTag : c.WeakTypeTag[TF]) : c.Tree =
        TwoFaceMaterializer.toNumValue(tf, c.symbolOf[TF], c.weakTypeOf[T])
      def toNumValue2[TF, T](tf : c.Tree)(id : c.Tree)(implicit tTag : c.WeakTypeTag[T], tfTag : c.WeakTypeTag[TF]) : c.Tree =
        TwoFaceMaterializer.toNumValue(tf, c.symbolOf[TF], c.weakTypeOf[T])
    }
  }

  trait Char[T] extends Any with TwoFaceAny[std.Char, T] {
    def == (r : std.Char)(implicit tfs : Boolean.Shell2[==, T, std.Char, GetArg0, std.Char]) = tfs(this.getValue, r)
    def == (r : std.Int)(implicit tfs : Boolean.ShellI[==, T, std.Char, GetArg0, std.Int]) = tfs(this.getValue, r)
    def == (r : std.Long)(implicit tfs : Boolean.ShellL[==, T, std.Char, GetArg0, std.Long]) = tfs(this.getValue, r)
    def == (r : std.Float)(implicit tfs : Boolean.ShellF[==, T, std.Char, GetArg0, std.Float]) = tfs(this.getValue, r)
    def == (r : std.Double)(implicit tfs : Boolean.ShellD[==, T, std.Char, GetArg0, std.Double]) = tfs(this.getValue, r)

    def != (r : std.Char)(implicit tfs : Boolean.Shell2[!=, T, std.Char, GetArg0, std.Char]) = tfs(this.getValue, r)
    def != (r : std.Int)(implicit tfs : Boolean.ShellI[!=, T, std.Char, GetArg0, std.Int]) = tfs(this.getValue, r)
    def != (r : std.Long)(implicit tfs : Boolean.ShellL[!=, T, std.Char, GetArg0, std.Long]) = tfs(this.getValue, r)
    def != (r : std.Float)(implicit tfs : Boolean.ShellF[!=, T, std.Char, GetArg0, std.Float]) = tfs(this.getValue, r)
    def != (r : std.Double)(implicit tfs : Boolean.ShellD[!=, T, std.Char, GetArg0, std.Double]) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Int.Shell2[+, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Int.ShellI[+, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.ShellL[+, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.ShellF[+, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.ShellD[+, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Int.Shell2[-, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Int.ShellI[-, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.ShellL[-, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.ShellF[-, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.ShellD[-, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Int.Shell2[*, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Int.ShellI[*, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.ShellL[*, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.ShellF[*, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.ShellD[*, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Int.Shell2[/, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Int.ShellI[/, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.ShellL[/, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.ShellF[/, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.ShellD[/, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Int.Shell2[%, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Int.ShellI[%, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.ShellL[%, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.ShellF[%, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.ShellD[%, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.ShellI[<, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.ShellL[<, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.ShellF[<, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.ShellD[<, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.ShellI[>, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.ShellL[>, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.ShellF[>, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.ShellD[>, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.ShellI[<=, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.ShellL[<=, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.ShellF[<=, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.ShellD[<=, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.ShellI[>=, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.ShellL[>=, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.ShellF[>=, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.ShellD[>=, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.ShellI[==, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.ShellL[==, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.ShellF[==, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.ShellD[==, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.ShellI[!=, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.ShellL[!=, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.ShellF[!=, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.ShellD[!=, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Int.Shell1[Negate, T, std.Char]) = tfs(this.getValue)
    def toNat(implicit nat : SafeNat[ToNat[T]]) : nat.Out = nat.value
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Char]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Char]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Char]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Char]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Char]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value

    def simplify(implicit tfs : Char.Shell1[Id, T, std.Char]) = tfs(this.getValue)
  }
  final class _Char[T](val value : std.Char) extends AnyVal with Char[T] {
    @inline def getValue : std.Char = value
  }
  implicit object Char extends TwoFaceAny.Builder[Char, std.Char, Shell.One.Char, Shell.Two.Char, Shell.Three.Char] {
    def create[T](value : std.Char) : Char[T] = new _Char[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Char[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Char])
    implicit def apply[T <: std.Char, Out <: T](value : T) : Char[Out] = macro Builder.Macro.fromNumValue[Char[_]]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Char[T] = create[T](id.valueWide.asInstanceOf[std.Char])
    implicit def widen[T](tf : Char[T]) : Char[std.Char] = create[std.Char](tf.getValue)
    implicit def tf2Num[T <: std.Char](tf : Char[T]) : T = macro Builder.Macro.toNumValue[Char[_], T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Char](tf : Char[T])(implicit id : OpAuxChar[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[Char[_], Out]
    implicit def unsafeTF2Num(tf : Char[_]) : std.Char = macro Builder.Macro.toNumValue[Char[_], std.Char]
    implicit def unknownTF2Num(tf : Char[std.Char]) : std.Char = macro Builder.Macro.toNumValue[Char[_], std.Char]
  }

  trait Int[T] extends Any with TwoFaceAny[std.Int, T] {
    def == (r : std.Char)(implicit tfs : Boolean.ShellI[==, T, std.Int, GetArg0, std.Char]) = tfs(this.getValue, r)
    def == (r : std.Int)(implicit tfs : Boolean.Shell2[==, T, std.Int, GetArg0, std.Int]) = tfs(this.getValue, r)
    def == (r : std.Long)(implicit tfs : Boolean.ShellL[==, T, std.Int, GetArg0, std.Long]) = tfs(this.getValue, r)
    def == (r : std.Float)(implicit tfs : Boolean.ShellF[==, T, std.Int, GetArg0, std.Float]) = tfs(this.getValue, r)
    def == (r : std.Double)(implicit tfs : Boolean.ShellD[==, T, std.Int, GetArg0, std.Double]) = tfs(this.getValue, r)

    def != (r : std.Char)(implicit tfs : Boolean.ShellI[!=, T, std.Int, GetArg0, std.Char]) = tfs(this.getValue, r)
    def != (r : std.Int)(implicit tfs : Boolean.Shell2[!=, T, std.Int, GetArg0, std.Int]) = tfs(this.getValue, r)
    def != (r : std.Long)(implicit tfs : Boolean.ShellL[!=, T, std.Int, GetArg0, std.Long]) = tfs(this.getValue, r)
    def != (r : std.Float)(implicit tfs : Boolean.ShellF[!=, T, std.Int, GetArg0, std.Float]) = tfs(this.getValue, r)
    def != (r : std.Double)(implicit tfs : Boolean.ShellD[!=, T, std.Int, GetArg0, std.Double]) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Int.ShellI[+, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Int.Shell2[+, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.ShellL[+, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.ShellF[+, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.ShellD[+, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Int.ShellI[-, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Int.Shell2[-, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.ShellL[-, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.ShellF[-, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.ShellD[-, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Int.ShellI[*, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Int.Shell2[*, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.ShellL[*, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.ShellF[*, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.ShellD[*, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Int.ShellI[/, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Int.Shell2[/, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.ShellL[/, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.ShellF[/, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.ShellD[/, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Int.ShellI[%, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Int.Shell2[%, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.ShellL[%, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.ShellF[%, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.ShellD[%, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.ShellI[<, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.ShellL[<, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.ShellF[<, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.ShellD[<, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.ShellI[>, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.ShellL[>, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.ShellF[>, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.ShellD[>, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.ShellI[<=, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.ShellL[<=, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.ShellF[<=, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.ShellD[<=, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.ShellI[>=, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.ShellL[>=, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.ShellF[>=, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.ShellD[>=, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.ShellI[==, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.ShellL[==, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.ShellF[==, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.ShellD[==, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.ShellI[!=, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.ShellL[!=, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.ShellF[!=, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.ShellD[!=, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Int.Shell1[Negate, T, std.Int]) = tfs(this.getValue)
    def toNat(implicit nat : SafeNat[ToNat[T]]) : nat.Out = nat.value
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Int]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Int]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Int]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Int]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Int]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value

    def simplify(implicit tfs : Int.Shell1[Id, T, std.Int]) = tfs(this.getValue)
  }
  final class _Int[T](val value : std.Int) extends AnyVal with Int[T] {
    @inline def getValue : std.Int = value
  }
  implicit object Int extends TwoFaceAny.Builder[Int, std.Int, Shell.One.Int, Shell.Two.Int, Shell.Three.Int] {
    def numberOfLeadingZeros[T](t : Int[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Int]) = tfs(t.getValue)
    def create[T](value : std.Int) : Int[T] = new _Int[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Int[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Int])
    implicit def apply[T <: std.Int, Out <: T](value : T) : Int[Out] = macro Builder.Macro.fromNumValue[Int[_]]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Int[T] = create[T](id.valueWide.asInstanceOf[std.Int])
    implicit def widen[T](tf : Int[T]) : Int[std.Int] = create[std.Int](tf.getValue)
    implicit def tf2Num[T <: std.Int](tf : Int[T]) : T = macro Builder.Macro.toNumValue[Int[_], T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Int](tf : Int[T])(implicit id : OpAuxInt[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[Int[_], Out]
    implicit def unsafeTF2Num(tf : Int[_]) : std.Int = macro Builder.Macro.toNumValue[Int[_], std.Int]
    implicit def unknownTF2Num(tf : Int[std.Int]) : std.Int = macro Builder.Macro.toNumValue[Int[_], std.Int]
  }

  trait Long[T] extends Any with TwoFaceAny[std.Long, T] {
    def == (r : std.Char)(implicit tfs : Boolean.ShellL[==, T, std.Long, GetArg0, std.Char]) = tfs(this.getValue, r)
    def == (r : std.Int)(implicit tfs : Boolean.ShellL[==, T, std.Long, GetArg0, std.Int]) = tfs(this.getValue, r)
    def == (r : std.Long)(implicit tfs : Boolean.Shell2[==, T, std.Long, GetArg0, std.Long]) = tfs(this.getValue, r)
    def == (r : std.Float)(implicit tfs : Boolean.ShellF[==, T, std.Long, GetArg0, std.Float]) = tfs(this.getValue, r)
    def == (r : std.Double)(implicit tfs : Boolean.ShellD[==, T, std.Long, GetArg0, std.Double]) = tfs(this.getValue, r)

    def != (r : std.Char)(implicit tfs : Boolean.ShellL[!=, T, std.Long, GetArg0, std.Char]) = tfs(this.getValue, r)
    def != (r : std.Int)(implicit tfs : Boolean.ShellL[!=, T, std.Long, GetArg0, std.Int]) = tfs(this.getValue, r)
    def != (r : std.Long)(implicit tfs : Boolean.Shell2[!=, T, std.Long, GetArg0, std.Long]) = tfs(this.getValue, r)
    def != (r : std.Float)(implicit tfs : Boolean.ShellF[!=, T, std.Long, GetArg0, std.Float]) = tfs(this.getValue, r)
    def != (r : std.Double)(implicit tfs : Boolean.ShellD[!=, T, std.Long, GetArg0, std.Double]) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Long.ShellL[+, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Long.ShellL[+, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.ShellF[+, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.ShellD[+, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Long.ShellL[-, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Long.ShellL[-, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.ShellF[-, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.ShellD[-, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Long.ShellL[*, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Long.ShellL[*, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.ShellF[*, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.ShellD[*, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Long.ShellL[/, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Long.ShellL[/, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.ShellF[/, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.ShellD[/, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Long.ShellL[%, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Long.ShellL[%, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.ShellF[%, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.ShellD[%, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.ShellL[<, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.ShellL[<, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.ShellF[<, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.ShellD[<, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.ShellL[>, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.ShellL[>, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.ShellF[>, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.ShellD[>, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.ShellL[<=, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.ShellL[<=, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.ShellF[<=, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.ShellD[<=, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.ShellL[>=, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.ShellL[>=, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.ShellF[>=, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.ShellD[>=, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.ShellL[==, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.ShellL[==, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.ShellF[==, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.ShellD[==, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.ShellL[!=, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.ShellL[!=, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.ShellF[!=, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.ShellD[!=, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Long.Shell1[Negate, T, std.Long]) = tfs(this.getValue)
    def toNat(implicit nat : SafeNat[ToNat[T]]) : nat.Out = nat.value
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Long]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Long]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Long]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Long]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Long]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value

    def simplify(implicit tfs : Long.Shell1[Id, T, std.Long]) = tfs(this.getValue)
  }

  final class _Long[T](val value : std.Long) extends AnyVal with Long[T] {
    @inline def getValue : std.Long = value
  }
  implicit object Long extends TwoFaceAny.Builder[Long, std.Long, Shell.One.Long, Shell.Two.Long, Shell.Three.Long] {
    def numberOfLeadingZeros[T](t : Long[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Long]) = tfs(t.getValue)
    def create[T](value : std.Long) : Long[T] = new _Long[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Long[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Long])
    implicit def apply[T <: std.Long, Out <: T](value : T) : Long[Out] = macro Builder.Macro.fromNumValue[Long[_]]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Long[T] = create[T](id.valueWide.asInstanceOf[std.Long])
    implicit def widen[T](tf : Long[T]) : Long[std.Long] = create[std.Long](tf.getValue)
    implicit def tf2Num[T <: std.Long](tf : Long[T]) : T = macro Builder.Macro.toNumValue[Long[_], T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Long](tf : Long[T])(implicit id : OpAuxLong[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[Long[_], Out]
    implicit def unsafeTF2Num(tf : Long[_]) : std.Long = macro Builder.Macro.toNumValue[Long[_], std.Long]
    implicit def unknownTF2Num(tf : Long[std.Long]) : std.Long = macro Builder.Macro.toNumValue[Long[_], std.Long]
  }

  trait Float[T] extends Any with TwoFaceAny[std.Float, T] {
    def == (r : std.Char)(implicit tfs : Boolean.ShellF[==, T, std.Float, GetArg0, std.Char]) = tfs(this.getValue, r)
    def == (r : std.Int)(implicit tfs : Boolean.ShellF[==, T, std.Float, GetArg0, std.Int]) = tfs(this.getValue, r)
    def == (r : std.Long)(implicit tfs : Boolean.ShellF[==, T, std.Float, GetArg0, std.Long]) = tfs(this.getValue, r)
    def == (r : std.Float)(implicit tfs : Boolean.Shell2[==, T, std.Float, GetArg0, std.Float]) = tfs(this.getValue, r)
    def == (r : std.Double)(implicit tfs : Boolean.ShellD[==, T, std.Float, GetArg0, std.Double]) = tfs(this.getValue, r)

    def != (r : std.Char)(implicit tfs : Boolean.ShellF[!=, T, std.Float, GetArg0, std.Char]) = tfs(this.getValue, r)
    def != (r : std.Int)(implicit tfs : Boolean.ShellF[!=, T, std.Float, GetArg0, std.Int]) = tfs(this.getValue, r)
    def != (r : std.Long)(implicit tfs : Boolean.ShellF[!=, T, std.Float, GetArg0, std.Long]) = tfs(this.getValue, r)
    def != (r : std.Float)(implicit tfs : Boolean.Shell2[!=, T, std.Float, GetArg0, std.Float]) = tfs(this.getValue, r)
    def != (r : std.Double)(implicit tfs : Boolean.ShellD[!=, T, std.Float, GetArg0, std.Double]) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Float.ShellF[+, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Float.ShellF[+, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Float.ShellF[+, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.ShellD[+, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Float.ShellF[-, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Float.ShellF[-, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Float.ShellF[-, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.ShellD[-, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Float.ShellF[*, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Float.ShellF[*, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Float.ShellF[*, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.ShellD[*, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Float.ShellF[/, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Float.ShellF[/, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Float.ShellF[/, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.ShellD[/, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Float.ShellF[%, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Float.ShellF[%, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Float.ShellF[%, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.ShellD[%, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.ShellF[<, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.ShellF[<, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.ShellF[<, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.ShellD[<, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.ShellF[>, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.ShellF[>, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.ShellF[>, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.ShellD[>, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.ShellF[<=, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.ShellF[<=, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.ShellF[<=, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.ShellD[<=, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.ShellF[>=, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.ShellF[>=, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.ShellF[>=, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.ShellD[>=, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.ShellF[==, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.ShellF[==, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.ShellF[==, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.ShellD[==, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.ShellF[!=, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.ShellF[!=, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.ShellF[!=, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.ShellD[!=, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Float.Shell1[Negate, T, std.Float]) = tfs(this.getValue)
    def toNat(implicit nat : SafeNat[ToNat[T]]) : nat.Out = nat.value
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Float]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Float]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Float]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Float]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Float]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value

    def simplify(implicit tfs : Float.Shell1[Id, T, std.Float]) = tfs(this.getValue)
  }
  final class _Float[T](val value : std.Float) extends AnyVal with Float[T] {
    @inline def getValue : std.Float = value
  }
  implicit object Float extends TwoFaceAny.Builder[Float, std.Float, Shell.One.Float, Shell.Two.Float, Shell.Three.Float] {
    def create[T](value : std.Float) : Float[T] = new _Float[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Float[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Float])
    implicit def apply[T <: std.Float, Out <: T](value : T) : Float[Out] = macro Builder.Macro.fromNumValue[Float[_]]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Float[T] = create[T](id.valueWide.asInstanceOf[std.Float])
    implicit def widen[T](tf : Float[T]) : Float[std.Float] = create[std.Float](tf.getValue)
    implicit def tf2Num[T <: std.Float](tf : Float[T]) : T = macro Builder.Macro.toNumValue[Float[_], T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Float](tf : Float[T])(implicit id : OpAuxFloat[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[Float[_], Out]
    implicit def unsafeTF2Num(tf : Float[_]) : std.Float = macro Builder.Macro.toNumValue[Float[_], std.Float]
    implicit def unknownTF2Num(tf : Float[std.Float]) : std.Float = macro Builder.Macro.toNumValue[Float[_], std.Float]
  }

  trait Double[T] extends Any with TwoFaceAny[std.Double, T] {
    def == (r : std.Char)(implicit tfs : Boolean.ShellD[==, T, std.Double, GetArg0, std.Char]) = tfs(this.getValue, r)
    def == (r : std.Int)(implicit tfs : Boolean.ShellD[==, T, std.Double, GetArg0, std.Int]) = tfs(this.getValue, r)
    def == (r : std.Long)(implicit tfs : Boolean.ShellD[==, T, std.Double, GetArg0, std.Long]) = tfs(this.getValue, r)
    def == (r : std.Float)(implicit tfs : Boolean.ShellD[==, T, std.Double, GetArg0, std.Float]) = tfs(this.getValue, r)
    def == (r : std.Double)(implicit tfs : Boolean.Shell2[==, T, std.Double, GetArg0, std.Double]) = tfs(this.getValue, r)

    def != (r : std.Char)(implicit tfs : Boolean.ShellD[!=, T, std.Double, GetArg0, std.Char]) = tfs(this.getValue, r)
    def != (r : std.Int)(implicit tfs : Boolean.ShellD[!=, T, std.Double, GetArg0, std.Int]) = tfs(this.getValue, r)
    def != (r : std.Long)(implicit tfs : Boolean.ShellD[!=, T, std.Double, GetArg0, std.Long]) = tfs(this.getValue, r)
    def != (r : std.Float)(implicit tfs : Boolean.ShellD[!=, T, std.Double, GetArg0, std.Float]) = tfs(this.getValue, r)
    def != (r : std.Double)(implicit tfs : Boolean.Shell2[!=, T, std.Double, GetArg0, std.Double]) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Double.ShellD[+, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Double.ShellD[+, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Double.ShellD[+, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Double.ShellD[+, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Double.ShellD[-, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Double.ShellD[-, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Double.ShellD[-, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Double.ShellD[-, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Double.ShellD[*, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Double.ShellD[*, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Double.ShellD[*, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Double.ShellD[*, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Double.ShellD[/, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Double.ShellD[/, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Double.ShellD[/, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Double.ShellD[/, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Double.ShellD[%, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Double.ShellD[%, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Double.ShellD[%, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Double.ShellD[%, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.ShellD[<, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.ShellD[<, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.ShellD[<, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.ShellD[<, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.ShellD[>, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.ShellD[>, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.ShellD[>, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.ShellD[>, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.ShellD[<=, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.ShellD[<=, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.ShellD[<=, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.ShellD[<=, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.ShellD[>=, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.ShellD[>=, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.ShellD[>=, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.ShellD[>=, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.ShellD[==, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.ShellD[==, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.ShellD[==, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.ShellD[==, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.ShellD[!=, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.ShellD[!=, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.ShellD[!=, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.ShellD[!=, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Double.Shell1[Negate, T, std.Double]) = tfs(this.getValue)
    def toNat(implicit nat : SafeNat[ToNat[T]]) : nat.Out = nat.value
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Double]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Double]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Double]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Double]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Double]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value

    def simplify(implicit tfs : Double.Shell1[Id, T, std.Double]) = tfs(this.getValue)
  }
  final class _Double[T](val value : std.Double) extends AnyVal with Double[T] {
    @inline def getValue : std.Double = value
  }
  implicit object Double extends TwoFaceAny.Builder[Double, std.Double, Shell.One.Double, Shell.Two.Double, Shell.Three.Double] {
    def create[T](value : std.Double) : Double[T] = new _Double[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Double[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Double])
    implicit def apply[T <: std.Double, Out <: T](value : T) : Double[Out] = macro Builder.Macro.fromNumValue[Double[_]]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Double[T] = create[T](id.valueWide.asInstanceOf[std.Double])
    implicit def widen[T](tf : Double[T]) : Double[std.Double] = create[std.Double](tf.getValue)
    implicit def tf2Num[T <: std.Double](tf : Double[T]) : T = macro Builder.Macro.toNumValue[Double[_], T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Double](tf : Double[T])(implicit id : OpAuxDouble[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[Double[_], Out]
    implicit def unsafeTF2Num(tf : Double[_]) : std.Double = macro Builder.Macro.toNumValue[Double[_], std.Double]
    implicit def unknownTF2Num(tf : Double[std.Double]) : std.Double = macro Builder.Macro.toNumValue[Double[_], std.Double]
  }

  trait String[T] extends Any with TwoFaceAny[std.String, T] {
    def == (r : std.String)(implicit tfs : Boolean.Shell2[==, T, std.String, GetArg0, std.String]) = tfs(this.getValue, r)
    def != (r : std.String)(implicit tfs : Boolean.Shell2[!=, T, std.String, GetArg0, std.String]) = tfs(this.getValue, r)

    def +  [R](r : String[R])(implicit tfs : String.Shell2[+, T, std.String, R, std.String]) = tfs(this.getValue, r.getValue)
    def == [R](r : String[R])(implicit tfs : Boolean.Shell2[==, T, std.String, R, std.String]) = tfs(this.getValue, r.getValue)
    def != [R](r : String[R])(implicit tfs : Boolean.Shell2[!=, T, std.String, R, std.String]) = tfs(this.getValue, r.getValue)

    def reverse(implicit tfs : String.Shell1[Reverse, T, std.String]) = tfs(this.getValue)
    def substring[R](r : Int[R])(implicit tfs : String.Shell2[Substring, T, std.String, R, std.Int]) = tfs(this.getValue, r.getValue)
    def length(implicit tfs : Int.Shell1[Length, T, std.String]) = tfs(this.getValue)
    def charAt[R](r : Int[R])(implicit tfs : Char.Shell2[CharAt, T, std.String, R, std.Int]) = tfs(this.getValue, r.getValue)
    def toNat(implicit nat : SafeNat[ToNat[T]]) : nat.Out = nat.value
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.String]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.String]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.String]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.String]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value
 
    def simplify(implicit tfs : String.Shell1[Id, T, std.String]) = tfs(this.getValue)
  }
  final class _String[T](val value : std.String) extends AnyVal with String[T] {
    @inline def getValue : std.String = value
  }
  implicit object String extends TwoFaceAny.Builder[String, std.String, Shell.One.String, Shell.Two.String, Shell.Three.String] {
    def create[T](value : std.String) : String[T] = new _String[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : String[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.String])
    implicit def apply[T <: std.String, Out <: T](value : T) : String[Out] = macro Builder.Macro.fromNumValue[String[_]]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : String[T] = create[T](id.valueWide.asInstanceOf[std.String])
    implicit def widen[T](tf : String[T]) : String[std.String] = create[std.String](tf.getValue)
    implicit def tf2Num[T <: std.String](tf : String[T]) : T = macro Builder.Macro.toNumValue[String[_], T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.String](tf : String[T])(implicit id : OpAuxString[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[String[_], Out]
    implicit def unsafeTF2Num(tf : String[_]) : std.String = macro Builder.Macro.toNumValue[String[_], std.String]
    implicit def unknownTF2Num(tf : String[std.String]) : std.String = macro Builder.Macro.toNumValue[String[_], std.String]
  }

  trait Boolean[T] extends Any with TwoFaceAny[std.Boolean, T] {
    def == (r : std.Boolean)(implicit tfs : Boolean.Shell2[==, T, std.Boolean, GetArg0, std.Boolean]) = tfs(this.getValue, r)
    def != (r : std.Boolean)(implicit tfs : Boolean.Shell2[!=, T, std.Boolean, GetArg0, std.Boolean]) = tfs(this.getValue, r)

    def == [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[==, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def != [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[!=, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def && [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[&&, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def || [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[||, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def unary_!(implicit tfs : Boolean.Shell1[!, T, std.Boolean]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Boolean]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value

    def simplify(implicit tfs : Boolean.Shell1[Id, T, std.Boolean]) = tfs(this.getValue)
  }
  final class _Boolean[T](val value : std.Boolean) extends AnyVal with Boolean[T] {
    @inline def getValue : std.Boolean = value
  }
  implicit object Boolean extends TwoFaceAny.Builder[Boolean, std.Boolean, Shell.One.Boolean, Shell.Two.Boolean, Shell.Three.Boolean] {
    def create[T](value : std.Boolean) : Boolean[T] = new _Boolean[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Boolean[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Boolean])
    implicit def apply[T <: std.Boolean, Out <: T](value : T) : Boolean[Out] = macro Builder.Macro.fromNumValue[Boolean[_]]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Boolean[T] = create[T](id.valueWide.asInstanceOf[std.Boolean])
    implicit def widen[T](tf : Boolean[T]) : Boolean[std.Boolean] = create[std.Boolean](tf.getValue)
    implicit def tf2Num[T <: std.Boolean](tf : Boolean[T]) : T = macro Builder.Macro.toNumValue[Boolean[_], T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Boolean](tf : Boolean[T])(implicit id : OpAuxBoolean[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[Boolean[_], Out]
    implicit def unsafeTF2Num(tf : Boolean[_]) : std.Boolean = macro Builder.Macro.toNumValue[Boolean[_], std.Boolean]
    implicit def unknownTF2Num(tf : Boolean[std.Boolean]) : std.Boolean = macro Builder.Macro.toNumValue[Boolean[_], std.Boolean]

    //////////////////////////////////////////////////////////////////
    type RequireShell[Cond, Msg, Sym] =
      Shell.Two.Boolean[RequireMsgSym[Cond, Msg, Sym],
        RequireMsgSym[Arg[W.`1`.T, Cond, std.Boolean], Arg[W.`2`.T, Msg, std.String], Sym],
        Cond, std.Boolean, Msg, std.String]
    //////////////////////////////////////////////////////////////////
  }

}
