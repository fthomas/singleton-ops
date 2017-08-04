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

  trait CharLike extends Any with TwoFaceAny[std.Char] {
    def == (r : std.Char) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Int) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Long) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Float) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Double) = Boolean.create[std.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Char, R, std.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Char, R, std.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Char, R, std.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Char, R, std.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Char, R, std.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

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
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Char]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Char]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Char]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Char]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Char]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Char[id.Out] =
//      Char.create[id.Out](id.valueWide.asInstanceOf[std.Char])
  }
  final class _Char[T0](val value : std.Char) extends AnyVal with CharLike {
    type T = T0
    @inline def getValue : std.Char = value
  }
  implicit object CharLike extends TwoFaceAny.Builder[CharLike, std.Char, Shell.One.Char, Shell.Two.Char, Shell.Three.Char] {
    def create[T](value : std.Char) = new _Char[T](value)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Char[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Char])
    implicit def apply[T <: std.Char](value : T) : Char[T] = macro Builder.Macro.fromNumValue[CharLike]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Char[T] = create[T](id.valueWide.asInstanceOf[std.Char])
    implicit def tf2Num[T <: std.Char](tf : Char[T]) : T = macro Builder.Macro.toNumValue[CharLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op](tf : Char[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.Out = macro Builder.Macro.toNumValue2[CharLike, id.Out]
    implicit def unknownTF2Num(tf : CharLike) : std.Char = macro Builder.Macro.toNumValue[CharLike, std.Char]
  }

  trait IntLike extends Any with TwoFaceAny[std.Int] {
    def == (r : std.Char) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Int) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Long) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Float) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Double) = Boolean.create[std.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Int, R, std.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Int, R, std.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Int, R, std.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Int, R, std.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Int, R, std.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

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
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Int]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Int]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Int]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Int]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Int]) = tfs(this.getValue)

    def simplify(implicit tfs : Int.Shell1[Id, T, std.Int]) = tfs(this.getValue)
  }
  final class _Int[T0](val value : std.Int) extends AnyVal with IntLike {
    type T = T0
    @inline def getValue : std.Int = value
  }
  implicit object IntLike extends TwoFaceAny.Builder[IntLike, std.Int, Shell.One.Int, Shell.Two.Int, Shell.Three.Int] {
    def create[T](value : std.Int) = new _Int[T](value)
    def numberOfLeadingZeros[T](t : Int[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Int]) = tfs(t.getValue)
    def apply[T](implicit id : AcceptNonLiteral[Id[T]]) : Int[id.Out] = create[id.Out](id.valueWide.asInstanceOf[std.Int])
    implicit def apply[T <: std.Int](value : T) : Int[T] = macro Builder.Macro.fromNumValue[IntLike]
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Int[T] = create[T](id.valueWide.asInstanceOf[std.Int])
    implicit def tf2Num[T <: std.Int](tf : Int[T]) : T = macro Builder.Macro.toNumValue[IntLike, T]
    implicit def opTF2Num[T <: singleton.ops.impl.Op, Out <: std.Int](tf : Int[T])(implicit id : OpAuxInt[AcceptNonLiteral[Id[T]], Out]) : Out = macro Builder.Macro.toNumValue2[IntLike, Out]
    implicit def unknownTF2Num(tf : IntLike) : std.Int = macro Builder.Macro.toNumValue[IntLike, std.Int]
  }

  trait LongLike extends Any with TwoFaceAny[std.Long] {
    def == (r : std.Char) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Int) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Long) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Float) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Double) = Boolean.create[std.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Long, R, std.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Long, R, std.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Long, R, std.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Long, R, std.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Long, R, std.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)
//
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
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Long]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Long]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Long]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Long]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Long]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Long[id.Out] =
//      Long.create[id.Out](id.valueWide.asInstanceOf[std.Long])
  }

  final class _Long[T0](val value : std.Long) extends AnyVal with LongLike {
    type T = T0
    @inline def getValue : std.Long = value
  }
  implicit object LongLike extends TwoFaceAny.Builder[LongLike, std.Long, Shell.One.Long, Shell.Two.Long, Shell.Three.Long] {
    def create[T](value : std.Long) = new _Long[T](value)
    def numberOfLeadingZeros[T](t : Long[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Long]) = tfs(t.getValue)
  }

  trait FloatLike extends Any with TwoFaceAny[std.Float] {
    def == (r : std.Char) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Int) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Long) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Float) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Double) = Boolean.create[std.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Float, R, std.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Float, R, std.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Float, R, std.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Float, R, std.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Float, R, std.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

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
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Float]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Float]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Float]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.Float]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Float]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Float[id.Out] =
//      Float.create[id.Out](id.valueWide.asInstanceOf[std.Float])
  }
  final class _Float[T0](val value : std.Float) extends AnyVal with FloatLike {
    type T = T0
    @inline def getValue : std.Float = value
  }
  implicit object FloatLike extends TwoFaceAny.Builder[FloatLike, std.Float, Shell.One.Float, Shell.Two.Float, Shell.Three.Float] {
    def create[T](value : std.Float) = new _Float[T](value)
  }

  trait DoubleLike extends Any with TwoFaceAny[std.Double] {
    def == (r : std.Char) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Int) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Long) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Float) = Boolean.create[std.Boolean](this.getValue == r)
    def == (r : std.Double) = Boolean.create[std.Boolean](this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Double, R, std.Char]
    ) = tfs(this.getValue, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Double, R, std.Int],
      di1 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Double, R, std.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Double, R, std.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this.getValue, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Double, R, std.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this.getValue, r)

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
    def toChar(implicit tfs : Char.Shell1[ToChar, T, std.Double]) = tfs(this.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.Double]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.Double]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.Double]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Double]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Double[id.Out] =
//      Double.create[id.Out](id.valueWide.asInstanceOf[std.Double])
  }
  final class _Double[T0](val value : std.Double) extends AnyVal with DoubleLike {
    type T = T0
    @inline def getValue : std.Double = value
  }
  implicit object DoubleLike extends TwoFaceAny.Builder[DoubleLike, std.Double, Shell.One.Double, Shell.Two.Double, Shell.Three.Double] {
    def create[T](value : std.Double) = new _Double[T](value)
  }

  trait StringLike extends Any with TwoFaceAny[std.String] {
    def == (r : std.String) = Boolean.create[std.Boolean](this.getValue == r)
    def == [R <: XString](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.String, R, std.String]
    ) = tfs(this.getValue, r)

    def +  [R](r : String[R])(implicit tfs : String.Shell2[+, T, std.String, R, std.String]) = tfs(this.getValue, r.getValue)
    def == [R](r : String[R])(implicit tfs : Boolean.Shell2[==, T, std.String, R, std.String]) = tfs(this.getValue, r.getValue)
    def != [R](r : String[R])(implicit tfs : Boolean.Shell2[!=, T, std.String, R, std.String]) = tfs(this.getValue, r.getValue)

    def reverse(implicit tfs : String.Shell1[Reverse, T, std.String]) = tfs(this.getValue)
    def substring[R](r : Int[R])(implicit tfs : String.Shell2[Substring, T, std.String, R, std.Int]) = tfs(this.getValue, r.getValue)
    def length(implicit tfs : Int.Shell1[Length, T, std.String]) = tfs(this.getValue)
    def charAt[R](r : Int[R])(implicit tfs : Char.Shell2[CharAt, T, std.String, R, std.Int]) = tfs(this.getValue, r.getValue)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, std.String]) = tfs(this.getValue)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, std.String]) = tfs(this.getValue)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, std.String]) = tfs(this.getValue)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, std.String]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : String[id.Out] =
//      String.create[id.Out](id.valueWide.asInstanceOf[std.String])
  }
  final class _String[T0](val value : std.String) extends AnyVal with StringLike {
    type T = T0
    @inline def getValue : std.String = value
  }
  implicit object StringLike extends TwoFaceAny.Builder[StringLike, std.String, Shell.One.String, Shell.Two.String, Shell.Three.String] {
    def create[T](value : std.String) = new _String[T](value)
  }

  trait BooleanLike extends Any with TwoFaceAny[std.Boolean] {
    def == (r : std.Boolean) = Boolean.create[std.Boolean](this.getValue == r)
    def == [R <: XBoolean](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, std.Boolean, R, std.Boolean]
    ) = tfs(this.getValue, r)
    def == [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[==, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def != [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[!=, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def && [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[&&, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def || [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[||, T, std.Boolean, R, std.Boolean]) = tfs(this.getValue, r.getValue)
    def unary_!(implicit tfs : Boolean.Shell1[!, T, std.Boolean]) = tfs(this.getValue)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, std.Boolean]) = tfs(this.getValue)

//    def simplify(implicit id : AcceptNonLiteral[this.type]) : Boolean[id.Out] =
//      Boolean.create[id.Out](id.valueWide.asInstanceOf[std.Boolean])
  }
  final class _Boolean[T0](val value : std.Boolean) extends AnyVal with BooleanLike {
    type T = T0
    @inline def getValue : std.Boolean = value
  }
  implicit object BooleanLike extends TwoFaceAny.Builder[BooleanLike, std.Boolean, Shell.One.Boolean, Shell.Two.Boolean, Shell.Three.Boolean] {
    def create[T](value : std.Boolean) = new _Boolean[T](value)
  }

}
