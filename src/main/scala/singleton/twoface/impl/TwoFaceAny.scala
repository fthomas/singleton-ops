package singleton.twoface.impl

import macrocompat.bundle
import singleton.ops._
import singleton.ops.impl.GeneralMacros
import singleton.twoface.TwoFace._

import scala.reflect.macros.whitebox

trait TwoFaceAny[Face] extends Any {
  type T
  def isLiteral(implicit rt : RunTime[T]) : scala.Boolean = !rt
  @inline def getValue : Face
  override def toString = getValue.toString
}

object TwoFaceAny {
  trait Builder[TF <: TwoFaceAny[Face], Face, Shl1[_,_,_,_], Shl2[_,_,_,_,_,_], Shl3[_,_,_,_,_,_,_,_]] {
    type Aux[T0] = TF{type T = T0}
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
    def create[T](value : Face) : Lt[T]

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
        TwoFaceMaterializer.toNumValue[T](tf, c.symbolOf[TF], c.weakTypeOf[T])
      def toNumValue2[TF, T](tf : c.Tree)(id : c.Tree)(implicit tTag : c.WeakTypeTag[T]) : c.Tree =
        TwoFaceMaterializer.toNumValue(tf, c.symbolOf[TF], c.weakTypeOf[T])
    }
  }

  trait CharLike extends Any with TwoFaceAny[scala.Char] {
    def == (r : scala.Char) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Int) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Long) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Float) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Double) = Boolean.create[scala.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Int.Shell2[+, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Int.Shell2[+, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Int.Shell2[-, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Int.Shell2[-, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Int.Shell2[*, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Int.Shell2[*, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Int.Shell2[/, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Int.Shell2[/, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Int.Shell2[%, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Int.Shell2[%, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Int.Shell1[Negate, T, scala.Char]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Char]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Char]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Char]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Char]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Char]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Char[id.Out] =
//      Char.create[id.Out](id.valueWide.asInstanceOf[scala.Char])
  }
  final class _Char[T0](val value : scala.Char) extends AnyVal with CharLike {
    type T = T0
    @inline def getValue : scala.Char = value
  }
  implicit object CharLike extends TwoFaceAny.Builder[CharLike, scala.Char, Shell.One.Char, Shell.Two.Char, Shell.Three.Char] {
    def create[T](value : scala.Char) = new _Char[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Char[id.Out] = create[id.Out](id.valueWide.asInstanceOf[scala.Char])
    implicit def apply[T <: scala.Char](value : T) : Char[T] = macro Builder.Macro.fromNumValue[CharLike]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Char[T] = create[T](id.valueWide.asInstanceOf[scala.Char])
    implicit def tf2Num[T <: scala.Char](tf : Char[T]) : T = macro Builder.Macro.toNumValue[CharLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op](tf : Char[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.Out = macro Builder.Macro.toNumValue2[CharLike, id.Out]
    implicit def unknownTF2Num(tf : CharLike) : scala.Char = macro Builder.Macro.toNumValue[CharLike, scala.Char]
  }

  trait IntLike extends Any with TwoFaceAny[scala.Int] {
    def == (r : scala.Char) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Int) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Long) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Float) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Double) = Boolean.create[scala.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Int.Shell2[+, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Int.Shell2[+, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Int.Shell2[-, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Int.Shell2[-, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Int.Shell2[*, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Int.Shell2[*, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Int.Shell2[/, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Int.Shell2[/, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Int.Shell2[%, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Int.Shell2[%, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Int.Shell1[Negate, T, scala.Int]) = tfs(this.getValue)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Int]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Int]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Int]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Int]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Int]) = tfs(this.getValue)

    def simplify(implicit tfs : Int.Shell1[Id, T, scala.Int]) = tfs(this.getValue)
  }
  final class _Int[T0](val value : scala.Int) extends AnyVal with IntLike {
    type T = T0
    @inline def getValue : scala.Int = value
  }
  implicit object IntLike extends TwoFaceAny.Builder[IntLike, scala.Int, Shell.One.Int, Shell.Two.Int, Shell.Three.Int] {
    def create[T](value : scala.Int) = new _Int[T](value)
    def numberOfLeadingZeros[T](t : Int[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, scala.Int]) = tfs(t.getValue)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Int[id.Out] = create[id.Out](id.valueWide.asInstanceOf[scala.Int])
    implicit def apply[T <: scala.Int](value : T) : Int[T] = macro Builder.Macro.fromNumValue[IntLike]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Int[T] = create[T](id.valueWide.asInstanceOf[scala.Int])
    implicit def tf2Num[T <: scala.Int](tf : Int[T]) : T = macro Builder.Macro.toNumValue[IntLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op](tf : Int[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.Out = macro Builder.Macro.toNumValue2[IntLike, id.Out]
    implicit def unknownTF2Num(tf : IntLike) : scala.Int = macro Builder.Macro.toNumValue[IntLike, scala.Int]
  }

  trait LongLike extends Any with TwoFaceAny[scala.Long] {
    def == (r : scala.Char) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Int) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Long) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Float) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Double) = Boolean.create[scala.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)
//
    def +  [R](r : Char[R])(implicit tfs : Long.Shell2[+, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Long.Shell2[+, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Long.Shell2[-, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Long.Shell2[-, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Long.Shell2[*, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Long.Shell2[*, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Long.Shell2[/, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Long.Shell2[/, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Long.Shell2[%, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Long.Shell2[%, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Long.Shell1[Negate, T, scala.Long]) = tfs(this.getValue)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Long]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Long]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Long]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Long]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Long]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Long[id.Out] =
//      Long.create[id.Out](id.valueWide.asInstanceOf[scala.Long])
  }

  final class _Long[T0](val value : scala.Long) extends AnyVal with LongLike {
    type T = T0
    @inline def getValue : scala.Long = value
  }
  implicit object LongLike extends TwoFaceAny.Builder[LongLike, scala.Long, Shell.One.Long, Shell.Two.Long, Shell.Three.Long] {
    def create[T](value : scala.Long) = new _Long[T](value)
    def numberOfLeadingZeros[T](t : Long[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, scala.Long]) = tfs(t.getValue)
  }

  trait FloatLike extends Any with TwoFaceAny[scala.Float] {
    def == (r : scala.Char) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Int) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Long) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Float) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Double) = Boolean.create[scala.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Float.Shell1[Negate, T, scala.Float]) = tfs(this.getValue)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Float]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Float]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Float]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Float]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Float]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Float[id.Out] =
//      Float.create[id.Out](id.valueWide.asInstanceOf[scala.Float])
  }
  final class _Float[T0](val value : scala.Float) extends AnyVal with FloatLike {
    type T = T0
    @inline def getValue : scala.Float = value
  }
  implicit object FloatLike extends TwoFaceAny.Builder[FloatLike, scala.Float, Shell.One.Float, Shell.Two.Float, Shell.Three.Float] {
    def create[T](value : scala.Float) = new _Float[T](value)
  }

  trait DoubleLike extends Any with TwoFaceAny[scala.Double] {
    def == (r : scala.Char) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Int) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Long) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Float) = Boolean.create[scala.Boolean](this.getValue == r)
    def == (r : scala.Double) = Boolean.create[scala.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

    def +  [R](r : Char[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Int[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Long[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Float[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Char[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Int[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Long[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Float[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Char[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Int[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Long[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Float[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Char[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Int[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Long[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Float[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Char[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Int[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Long[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Float[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Char]) = tfs(this.getValue, r.getValue)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Long]) = tfs(this.getValue, r.getValue)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Float]) = tfs(this.getValue, r.getValue)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Double]) = tfs(this.getValue, r.getValue)

    def unary_-(implicit tfs : Double.Shell1[Negate, T, scala.Double]) = tfs(this.getValue)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Double]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Double]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Double]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Double]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Double]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Double[id.Out] =
//      Double.create[id.Out](id.valueWide.asInstanceOf[scala.Double])
  }
  final class _Double[T0](val value : scala.Double) extends AnyVal with DoubleLike {
    type T = T0
    @inline def getValue : scala.Double = value
  }
  implicit object DoubleLike extends TwoFaceAny.Builder[DoubleLike, scala.Double, Shell.One.Double, Shell.Two.Double, Shell.Three.Double] {
    def create[T](value : scala.Double) = new _Double[T](value)
  }

  trait StringLike extends Any with TwoFaceAny[java.lang.String] {
    def == (r : java.lang.String) = Boolean.create[scala.Boolean](this.getValue == r)
    def == [R <: XString](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, java.lang.String, R, java.lang.String]
    ) = tfs(this.getValue, r)

    def +  [R](r : String[R])(implicit tfs : String.Shell2[+, T, java.lang.String, R, java.lang.String]) = tfs(this.getValue, r.getValue)
    def == [R](r : String[R])(implicit tfs : Boolean.Shell2[==, T, java.lang.String, R, java.lang.String]) = tfs(this.getValue, r.getValue)
    def != [R](r : String[R])(implicit tfs : Boolean.Shell2[!=, T, java.lang.String, R, java.lang.String]) = tfs(this.getValue, r.getValue)

    def reverse(implicit tfs : String.Shell1[Reverse, T, java.lang.String]) = tfs(this.getValue)
    def substring[R](r : Int[R])(implicit tfs : String.Shell2[Substring, T, java.lang.String, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def length(implicit tfs : Int.Shell1[Length, T, java.lang.String]) = tfs(this.getValue)
    def charAt[R](r : Int[R])(implicit tfs : Char.Shell2[CharAt, T, java.lang.String, R, scala.Int]) = tfs(this.getValue, r.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, java.lang.String]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, java.lang.String]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, java.lang.String]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, java.lang.String]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : String[id.Out] =
//      String.create[id.Out](id.valueWide.asInstanceOf[java.lang.String])
  }
  final class _String[T0](val value : java.lang.String) extends AnyVal with StringLike {
    type T = T0
    @inline def getValue : java.lang.String = value
  }
  implicit object StringLike extends TwoFaceAny.Builder[StringLike, java.lang.String, Shell.One.String, Shell.Two.String, Shell.Three.String] {
    def create[T](value : java.lang.String) = new _String[T](value)
  }

  trait BooleanLike extends Any with TwoFaceAny[scala.Boolean] {
    def == (r : scala.Boolean) = Boolean.create[scala.Boolean](this.getValue == r)
    def == [R <: XBoolean](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Boolean, R, scala.Boolean]
    ) = tfs(this.getValue, r)
    def == [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[==, T, scala.Boolean, R, scala.Boolean]) = tfs(this.getValue, r.getValue)
    def != [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Boolean, R, scala.Boolean]) = tfs(this.getValue, r.getValue)
    def && [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[&&, T, scala.Boolean, R, scala.Boolean]) = tfs(this.getValue, r.getValue)
    def || [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[||, T, scala.Boolean, R, scala.Boolean]) = tfs(this.getValue, r.getValue)
    def unary_!(implicit tfs : Boolean.Shell1[!, T, scala.Boolean]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Boolean]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Boolean[id.Out] =
//      Boolean.create[id.Out](id.valueWide.asInstanceOf[scala.Boolean])
  }
  final class _Boolean[T0](val value : scala.Boolean) extends AnyVal with BooleanLike {
    type T = T0
    @inline def getValue : scala.Boolean = value
  }
  implicit object BooleanLike extends TwoFaceAny.Builder[BooleanLike, scala.Boolean, Shell.One.Boolean, Shell.Two.Boolean, Shell.Three.Boolean] {
    def create[T](value : scala.Boolean) = new _Boolean[T](value)
  }

}
