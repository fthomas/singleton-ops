package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl.GeneralMacros
import singleton.twoface.TwoFace._
import singleton.ops.impl.std

import scala.reflect.macros.whitebox

trait TwoFaceAny[Face] extends Any {
  type T
  def isLiteral(implicit rt : RunTime[T]) : std.Boolean = !rt
  @inline def getValue : Face
  override def toString = getValue.toString
}

trait TwoFaceAnyUB[Face, T0] extends Any with TwoFaceAny[Face] {type T <: T0}

object TwoFaceAny {
  trait Builder[TF <: TwoFaceAny[Face], TFRet[T], Face, Shl1[_,_,_,_], Shl2[_,_,_,_,_,_], Shl3[_,_,_,_,_,_,_,_]] {
    type Ret[T] = TFRet[T]
    type Lt[T0] = TF{type T <: T0}
    type Shell1[Func[_], Arg1, Arg1Wide] =
      Shl1[Func[Arg1],
           Func[Arg[W.`1`.T, Arg1, Arg1Wide]],
           Arg1, Arg1Wide]
    type Shell2[Func[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shl2[Func[Arg1, Arg2],
           Func[Arg[W.`1`.T, Arg1, Arg1Wide], Arg[W.`2`.T, Arg2, Arg2Wide]],
           Arg1, Arg1Wide, Arg2, Arg2Wide]
    type Shell3[Func[_,_,_], Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide] =
      Shl3[Func[Arg1, Arg2, Arg3],
           Func[Arg[W.`1`.T, Arg1, Arg1Wide], Arg[W.`2`.T, Arg2, Arg2Wide], Arg[W.`3`.T, Arg3, Arg3Wide]],
           Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]
    def create[T](value : Face) : Ret[T]

    //The implicit conversion from numeric to TwoFace could have been implemented generically, like the following.
    //However, since IntelliJ marks everything red, it was preferred to implement it specifically, in the meantime.
    //implicit def apply[T <: Face](value : T) : Lt[T] = macro Builder.Macro.fromNumValue

  }

  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def fromNumValue[TF](value : c.Tree) : c.Tree =
        TwoFaceMaterializer.fromNumValue(value, c.symbolOf[TF])
      def toNumValue[TF, T](tf : c.Tree)(implicit tTag : c.WeakTypeTag[T]) : c.Tree =
        TwoFaceMaterializer.toNumValue(tf, c.symbolOf[TF], c.weakTypeOf[T])
      def toNumValue2[TF, T](tf : c.Tree)(id : c.Tree)(implicit tTag : c.WeakTypeTag[T]) : c.Tree =
        TwoFaceMaterializer.toNumValue(tf, c.symbolOf[TF], c.weakTypeOf[T])

      def equal(r : c.Tree) : c.Tree =
        TwoFaceMaterializer.equal(c.prefix.tree, r)
      def equal1(r : c.Tree)(di1 : c.Tree) : c.Tree =
        TwoFaceMaterializer.equal(c.prefix.tree, r)
      def equal2(r : c.Tree)(di1 : c.Tree, di2 : c.Tree) : c.Tree =
        TwoFaceMaterializer.equal(c.prefix.tree, r)
      def equal3(r : c.Tree)(di1 : c.Tree, di2 : c.Tree, di3 : c.Tree) : c.Tree =
        TwoFaceMaterializer.equal(c.prefix.tree, r)
      def equal4(r : c.Tree)(di1 : c.Tree, di2 : c.Tree, di3 : c.Tree, di4 : c.Tree) : c.Tree =
        TwoFaceMaterializer.equal(c.prefix.tree, r)

      def nequal(r : c.Tree) : c.Tree =
        TwoFaceMaterializer.nequal(c.prefix.tree, r)
      def nequal1(r : c.Tree)(di1 : c.Tree) : c.Tree =
        TwoFaceMaterializer.nequal(c.prefix.tree, r)
      def nequal2(r : c.Tree)(di1 : c.Tree, di2 : c.Tree) : c.Tree =
        TwoFaceMaterializer.nequal(c.prefix.tree, r)
      def nequal3(r : c.Tree)(di1 : c.Tree, di2 : c.Tree, di3 : c.Tree) : c.Tree =
        TwoFaceMaterializer.nequal(c.prefix.tree, r)
      def nequal4(r : c.Tree)(di1 : c.Tree, di2 : c.Tree, di3 : c.Tree, di4 : c.Tree) : c.Tree =
        TwoFaceMaterializer.nequal(c.prefix.tree, r)
    }
  }

  trait CharLike extends Any with TwoFaceAny[std.Char] {
    def == [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.equal
    def == [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal1
    def == [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal2
    def == [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal3
    def == [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal4

    def != [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.nequal
    def != [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal1
    def != [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal2
    def != [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal3
    def != [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal4

    def +  [R](r : Char[R])(implicit tfs : Int.Shell2[+, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Int.Shell2[+, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Int.Shell2[-, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Int.Shell2[-, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Int.Shell2[*, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Int.Shell2[*, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Int.Shell2[/, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Int.Shell2[/, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Int.Shell2[%, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Int.Shell2[%, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, std.Char, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, std.Char, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, std.Char, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, std.Char, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, std.Char, R, std.Double]) = tfs(this.getValue, r.getValue)

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
  final class _Char[T0](val value : std.Char) extends AnyVal with CharLike {
    type T = T0
    @inline def getValue : std.Char = value
  }
  implicit object CharLike extends TwoFaceAny.Builder[CharLike, _Char, std.Char, Shell.One.Char, Shell.Two.Char, Shell.Three.Char] {
    def create[T](value : std.Char) : Ret[T] = new _Char[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Ret[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Char])
    implicit def apply[T <: std.Char](value : T) : Char[T] = macro Builder.Macro.fromNumValue[CharLike]
    implicit def ev[T, TF[T0] >: Char[T0]](implicit id : AcceptNonLiteral[Id[T]]) : TF[T] = create[T](id.valueWide.asInstanceOf[std.Char])
    implicit def tf2Num[T <: std.Char](tf : Char[T]) : T = macro Builder.Macro.toNumValue[CharLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Char](tf : Char[T])(implicit id : OpAuxChar[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[CharLike, Out]
    implicit def unknownTF2Num(tf : CharLike) : std.Char = macro Builder.Macro.toNumValue[CharLike, std.Char]
  }

  trait IntLike extends Any with TwoFaceAny[std.Int] {
    def == [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.equal
    def == [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal1
    def == [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal2
    def == [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal3
    def == [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal4

    def != [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.nequal
    def != [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal1
    def != [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal2
    def != [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal3
    def != [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal4

    def +  [R](r : Char[R])(implicit tfs : Int.Shell2[+, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Int.Shell2[+, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Int.Shell2[-, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Int.Shell2[-, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Int.Shell2[*, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Int.Shell2[*, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Int.Shell2[/, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Int.Shell2[/, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Int.Shell2[%, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Int.Shell2[%, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, std.Int, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, std.Int, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, std.Int, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, std.Int, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, std.Int, R, std.Double]) = tfs(this.getValue, r.getValue)

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
  final class _Int[T0](val value : std.Int) extends AnyVal with IntLike with TwoFaceAnyUB[std.Int, T0] {
    type T = T0
    @inline def getValue : std.Int = value
  }
  implicit object IntLike extends TwoFaceAny.Builder[IntLike, _Int, std.Int, Shell.One.Int, Shell.Two.Int, Shell.Three.Int] {
    def numberOfLeadingZeros[T](t : Int[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Int]) = tfs(t.getValue)
    def create[T](value : std.Int) : Ret[T] = new _Int[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Ret[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Int])
    implicit def apply[T <: std.Int](value : T) : Int[T] = macro Builder.Macro.fromNumValue[IntLike]
    implicit def ev[T, TF[T0] >: Int[T0]](implicit id : AcceptNonLiteral[Id[T]]) : TF[T] = create[T](id.valueWide.asInstanceOf[std.Int])
    implicit def tf2Num[T <: std.Int](tf : Int[T]) : T = macro Builder.Macro.toNumValue[IntLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Int](tf : Int[T])(implicit id : OpAuxInt[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[IntLike, Out]
    implicit def unknownTF2Num(tf : IntLike) : std.Int = macro Builder.Macro.toNumValue[IntLike, std.Int]
  }

  trait LongLike extends Any with TwoFaceAny[std.Long] {
    def == [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.equal
    def == [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal1
    def == [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal2
    def == [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal3
    def == [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal4

    def != [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.nequal
    def != [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal1
    def != [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal2
    def != [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal3
    def != [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal4

    def +  [R](r : Char[R])(implicit tfs : Long.Shell2[+, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Long.Shell2[+, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Long.Shell2[-, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Long.Shell2[-, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Long.Shell2[*, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Long.Shell2[*, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Long.Shell2[/, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Long.Shell2[/, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Long.Shell2[%, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Long.Shell2[%, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, std.Long, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, std.Long, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, std.Long, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, std.Long, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, std.Long, R, std.Double]) = tfs(this.getValue, r.getValue)

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

  final class _Long[T0](val value : std.Long) extends AnyVal with LongLike {
    type T = T0
    @inline def getValue : std.Long = value
  }
  implicit object LongLike extends TwoFaceAny.Builder[LongLike, _Long, std.Long, Shell.One.Long, Shell.Two.Long, Shell.Three.Long] {
    def numberOfLeadingZeros[T](t : Long[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Long]) = tfs(t.getValue)
    def create[T](value : std.Long) : Ret[T] = new _Long[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Ret[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Long])
    implicit def apply[T <: std.Long](value : T) : Long[T] = macro Builder.Macro.fromNumValue[LongLike]
    implicit def ev[T, TF[T0] >: Long[T0]](implicit id : AcceptNonLiteral[Id[T]]) : TF[T] = create[T](id.valueWide.asInstanceOf[std.Long])
    implicit def tf2Num[T <: std.Long](tf : Long[T]) : T = macro Builder.Macro.toNumValue[LongLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Long](tf : Long[T])(implicit id : OpAuxLong[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[LongLike, Out]
    implicit def unknownTF2Num(tf : LongLike) : std.Long = macro Builder.Macro.toNumValue[LongLike, std.Long]
  }

  trait FloatLike extends Any with TwoFaceAny[std.Float] {
    def == [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.equal
    def == [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal1
    def == [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal2
    def == [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal3
    def == [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal4

    def != [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.nequal
    def != [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal1
    def != [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal2
    def != [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal3
    def != [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal4

    def +  [R](r : Char[R])(implicit tfs : Float.Shell2[+, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Float.Shell2[+, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Float.Shell2[+, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Float.Shell2[-, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Float.Shell2[-, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Float.Shell2[-, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Float.Shell2[*, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Float.Shell2[*, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Float.Shell2[*, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Float.Shell2[/, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Float.Shell2[/, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Float.Shell2[/, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Float.Shell2[%, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Float.Shell2[%, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Float.Shell2[%, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, std.Float, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, std.Float, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, std.Float, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, std.Float, R, std.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, std.Float, R, std.Double]) = tfs(this.getValue, r.getValue)

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
  final class _Float[T0](val value : std.Float) extends AnyVal with FloatLike {
    type T = T0
    @inline def getValue : std.Float = value
  }
  implicit object FloatLike extends TwoFaceAny.Builder[FloatLike, _Float, std.Float, Shell.One.Float, Shell.Two.Float, Shell.Three.Float] {
    def create[T](value : std.Float) : Ret[T] = new _Float[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Ret[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Float])
    implicit def apply[T <: std.Float](value : T) : Float[T] = macro Builder.Macro.fromNumValue[FloatLike]
    implicit def ev[T, TF[T0] >: Float[T0]](implicit id : AcceptNonLiteral[Id[T]]) : TF[T] = create[T](id.valueWide.asInstanceOf[std.Float])
    implicit def tf2Num[T <: std.Float](tf : Float[T]) : T = macro Builder.Macro.toNumValue[FloatLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Float](tf : Float[T])(implicit id : OpAuxFloat[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[FloatLike, Out]
    implicit def unknownTF2Num(tf : FloatLike) : std.Float = macro Builder.Macro.toNumValue[FloatLike, std.Float]
  }

  trait DoubleLike extends Any with TwoFaceAny[std.Double] {
    def == [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.equal
    def == [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal1
    def == [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal2
    def == [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal3
    def == [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.equal4

    def != [R <: std.Char, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.nequal
    def != [R <: std.Int, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal1
    def != [R <: std.Long, Out >: std.Boolean](r : R)(
      implicit di1 : DummyImplicit, di2 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal2
    def != [R <: std.Float, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal3
    def != [R <: std.Double, Out >: std.Boolean](r : R) (
      implicit di1 : DummyImplicit, di2 : DummyImplicit, di3 : DummyImplicit, di4 : DummyImplicit
    ) : Boolean[Out] = macro Builder.Macro.nequal4

    def +  [R](r : Char[R])(implicit tfs : Double.Shell2[+, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Double.Shell2[+, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Double.Shell2[+, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Double.Shell2[+, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Double.Shell2[-, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Double.Shell2[-, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Double.Shell2[-, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Double.Shell2[-, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Double.Shell2[*, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Double.Shell2[*, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Double.Shell2[*, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Double.Shell2[*, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Double.Shell2[/, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Double.Shell2[/, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Double.Shell2[/, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Double.Shell2[/, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Double.Shell2[%, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Double.Shell2[%, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Double.Shell2[%, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Double.Shell2[%, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, std.Double, R, std.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, std.Double, R, std.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, std.Double, R, std.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, std.Double, R, std.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, std.Double, R, std.Float]) = tfs(this.getValue, r.getValue)
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
  final class _Double[T0](val value : std.Double) extends AnyVal with DoubleLike {
    type T = T0
    @inline def getValue : std.Double = value
  }
  implicit object DoubleLike extends TwoFaceAny.Builder[DoubleLike, _Double, std.Double, Shell.One.Double, Shell.Two.Double, Shell.Three.Double] {
    def create[T](value : std.Double) : Ret[T] = new _Double[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Ret[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Double])
    implicit def apply[T <: std.Double](value : T) : Double[T] = macro Builder.Macro.fromNumValue[DoubleLike]
    implicit def ev[T, TF[T0] >: Double[T0]](implicit id : AcceptNonLiteral[Id[T]]) : TF[T] = create[T](id.valueWide.asInstanceOf[std.Double])
    implicit def tf2Num[T <: std.Double](tf : Double[T]) : T = macro Builder.Macro.toNumValue[DoubleLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Double](tf : Double[T])(implicit id : OpAuxDouble[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[DoubleLike, Out]
    implicit def unknownTF2Num(tf : DoubleLike) : std.Double = macro Builder.Macro.toNumValue[DoubleLike, std.Double]
  }

  trait StringLike extends Any with TwoFaceAny[std.String] {
    def == [R <: std.String, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.equal
    def != [R <: std.String, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.nequal

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
  final class _String[T0](val value : std.String) extends AnyVal with StringLike {
    type T = T0
    @inline def getValue : std.String = value
  }
  implicit object StringLike extends TwoFaceAny.Builder[StringLike, _String, std.String, Shell.One.String, Shell.Two.String, Shell.Three.String] {
    def create[T](value : std.String) : Ret[T] = new _String[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Ret[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.String])
    implicit def apply[T <: std.String](value : T) : String[T] = macro Builder.Macro.fromNumValue[StringLike]
    implicit def ev[T, TF[T0] >: String[T0]](implicit id : AcceptNonLiteral[Id[T]]) : TF[T] = create[T](id.valueWide.asInstanceOf[std.String])
    implicit def tf2Num[T <: std.String](tf : String[T]) : T = macro Builder.Macro.toNumValue[StringLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.String](tf : String[T])(implicit id : OpAuxString[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[StringLike, Out]
    implicit def unknownTF2Num(tf : StringLike) : std.String = macro Builder.Macro.toNumValue[StringLike, std.String]
  }

  trait BooleanLike extends Any with TwoFaceAny[std.Boolean] {
    def == [R <: std.Boolean, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.equal
    def != [R <: std.Boolean, Out >: std.Boolean](r : R)
    : Boolean[Out] = macro Builder.Macro.nequal

    def == [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[==, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def != [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[!=, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def && [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[&&, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def || [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[||, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def unary_!(implicit tfs : Boolean.Shell1[!, T, std.Boolean]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Boolean]) = tfs(this.getValue)
    def toSymbol(implicit sym : SafeSymbol[ToSymbol[T]]) : sym.Out = sym.value

    def simplify(implicit tfs : Boolean.Shell1[Id, T, std.Boolean]) = tfs(this.getValue)
  }
  final class _Boolean[T0](val value : std.Boolean) extends AnyVal with BooleanLike {
    type T = T0
    @inline def getValue : std.Boolean = value
  }
  implicit object BooleanLike extends TwoFaceAny.Builder[BooleanLike, _Boolean, std.Boolean, Shell.One.Boolean, Shell.Two.Boolean, Shell.Three.Boolean] {
    def create[T](value : std.Boolean) : Ret[T] = new _Boolean[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Ret[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Boolean])
    implicit def apply[T <: std.Boolean](value : T) : Boolean[T] = macro Builder.Macro.fromNumValue[BooleanLike]
    implicit def ev[T, TF[T0] >: Boolean[T0]](implicit id : AcceptNonLiteral[Id[T]]) : TF[T] = create[T](id.valueWide.asInstanceOf[std.Boolean])
    implicit def tf2Num[T <: std.Boolean](tf : Boolean[T]) : T = macro Builder.Macro.toNumValue[BooleanLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Boolean](tf : Boolean[T])(implicit id : OpAuxBoolean[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[BooleanLike, Out]
    implicit def unknownTF2Num(tf : BooleanLike) : std.Boolean = macro Builder.Macro.toNumValue[BooleanLike, std.Boolean]
  }

}
