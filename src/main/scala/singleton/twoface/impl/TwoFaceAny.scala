package singleton.twoface.impl

import singleton.ops._
import singleton.ops.impl._
import macrocompat.bundle
import scala.reflect.macros.whitebox

trait TwoFaceAny[Face] extends Any {
  type T0
  def isLiteral(implicit rt : RunTime[T0]) : scala.Boolean = !rt
  @inline def getValue : Face
  override def toString = getValue.toString
}

object TwoFaceAny {
//  @inline implicit def fromTwoFaceUnsafe[Face, T](tf : TwoFaceAny[Face, T]) : Face = tf.getValue
//  @inline implicit def fromTwoFaceSafe[Face, T <: Face with Singleton](tf : TwoFaceAny[Face, T])
//                                                                      (implicit sc: ValueOf[T]) : T {} = valueOf[T]
//
  trait Builder[TF, Face] {
    type Aux[T] = TF {type T0 = T}
    protected[twoface] def create[T](value : Face) : Aux[T]
    implicit def apply[T <: Face with Singleton](value : T)(implicit tfb : Builder[TF, Face]) : Aux[T] =
      tfb.create[T](value)
    implicit def apply[T <: Face](value : T)(implicit tfb : Builder[TF, Face], di: DummyImplicit) : Aux[Face] =
      tfb.create[Face](value)
    implicit def apply[T](implicit si : Id[T], tfb : Builder[TF, Face]) : Aux[T] =
      tfb.create[T](si.value.asInstanceOf[Face])
  }
//
//  sealed trait TwoFaceOp[TF[_], Face, OP] {
//    type FB <: AcceptNonLiteral[OP]
//    val fb : FB
//    def apply(op : => Face)(implicit tfb : Builder[TF, Face]) : TF[fb.Out] =
//      tfb.create[fb.Out](if (fb.isLiteral) fb.valueWide.asInstanceOf[Face] else op)
//  }
//  object TwoFaceOp {
//    implicit def ev[TF[_], Face, OP](implicit fb0 : AcceptNonLiteral[OP]) : TwoFaceOp[TF, Face, OP]{type FB = fb0.type} =
//      new TwoFaceOp[TF, Face, OP]{type FB = fb0.type; val fb : FB = fb0}
//  }
//
//  trait Char[T] extends Any with TwoFaceAny[scala.Char, T] {
//    def == [R <: XChar](r : R)(
//      implicit tfo : Boolean.Return[T == R]
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Char)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XInt](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Int)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XLong](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Long)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XFloat](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Float)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XDouble](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Double)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//      di9 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//
//    def +  [R](r : Char[R])(implicit tfo : Int.Return[T + R])        = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Char[R])(implicit tfo : Int.Return[T - R])        = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Char[R])(implicit tfo : Int.Return[T * R])        = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Char[R])(implicit tfo : Int.Return[T / R])        = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Char[R])(implicit tfo : Int.Return[T % R])        = tfo(this.getValue %  r.getValue)
//    def == [R](r : Char[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Char[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Char[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Char[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Char[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Char[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Int[R])(implicit tfo : Int.Return[T + R])         = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Int[R])(implicit tfo : Int.Return[T - R])         = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Int[R])(implicit tfo : Int.Return[T * R])         = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Int[R])(implicit tfo : Int.Return[T / R])         = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Int[R])(implicit tfo : Int.Return[T % R])         = tfo(this.getValue %  r.getValue)
//    def == [R](r : Int[R])(implicit tfo : Boolean.Return[T == R])    = tfo(this.getValue == r.getValue)
//    def != [R](r : Int[R])(implicit tfo : Boolean.Return[T != R])    = tfo(this.getValue != r.getValue)
//    def <  [R](r : Int[R])(implicit tfo : Boolean.Return[T <  R])    = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Int[R])(implicit tfo : Boolean.Return[T >  R])    = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Int[R])(implicit tfo : Boolean.Return[T <= R])    = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Int[R])(implicit tfo : Boolean.Return[T >= R])    = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Long[R])(implicit tfo : Long.Return[T + R])       = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Long[R])(implicit tfo : Long.Return[T - R])       = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Long[R])(implicit tfo : Long.Return[T * R])       = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Long[R])(implicit tfo : Long.Return[T / R])       = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Long[R])(implicit tfo : Long.Return[T % R])       = tfo(this.getValue %  r.getValue)
//    def == [R](r : Long[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Long[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Long[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Long[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Long[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Long[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Float[R])(implicit tfo : Float.Return[T + R])     = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Float[R])(implicit tfo : Float.Return[T - R])     = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Float[R])(implicit tfo : Float.Return[T * R])     = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Float[R])(implicit tfo : Float.Return[T / R])     = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Float[R])(implicit tfo : Float.Return[T % R])     = tfo(this.getValue %  r.getValue)
//    def == [R](r : Float[R])(implicit tfo : Boolean.Return[T == R])  = tfo(this.getValue == r.getValue)
//    def != [R](r : Float[R])(implicit tfo : Boolean.Return[T != R])  = tfo(this.getValue != r.getValue)
//    def <  [R](r : Float[R])(implicit tfo : Boolean.Return[T <  R])  = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Float[R])(implicit tfo : Boolean.Return[T >  R])  = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Float[R])(implicit tfo : Boolean.Return[T <= R])  = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Float[R])(implicit tfo : Boolean.Return[T >= R])  = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Double[R])(implicit tfo : Double.Return[T + R])   = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Double[R])(implicit tfo : Double.Return[T - R])   = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Double[R])(implicit tfo : Double.Return[T * R])   = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Double[R])(implicit tfo : Double.Return[T / R])   = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Double[R])(implicit tfo : Double.Return[T % R])   = tfo(this.getValue %  r.getValue)
//    def == [R](r : Double[R])(implicit tfo : Boolean.Return[T == R]) = tfo(this.getValue == r.getValue)
//    def != [R](r : Double[R])(implicit tfo : Boolean.Return[T != R]) = tfo(this.getValue != r.getValue)
//    def <  [R](r : Double[R])(implicit tfo : Boolean.Return[T <  R]) = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Double[R])(implicit tfo : Boolean.Return[T >  R]) = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Double[R])(implicit tfo : Boolean.Return[T <= R]) = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Double[R])(implicit tfo : Boolean.Return[T >= R]) = tfo(this.getValue >= r.getValue)
//    def unary_-            (implicit tfo : Int.Return[Negate[T]])    = tfo(-this.getValue)
//    def toInt(implicit tfo : Int.Return[ToInt[T]])                   = tfo(this.getValue.toInt)
//    def toLong(implicit tfo : Long.Return[ToLong[T]])                = tfo(this.getValue.toLong)
//    def toFloat(implicit tfo : Float.Return[ToFloat[T]])             = tfo(this.getValue.toFloat)
//    def toDouble(implicit tfo : Double.Return[ToDouble[T]])          = tfo(this.getValue.toDouble)
//    def toString(implicit tfo : String.Return[ToString[T]])          = tfo(this.getValue.toString)
//  }
//  final class _Char[T](val value : scala.Char) extends AnyVal with TwoFaceAny.Char[T] {
//    @inline def getValue : scala.Char = value
//  }
//  implicit object Char extends TwoFaceAny.Builder[Char, scala.Char] {
//    type Return[OP] = TwoFaceOp[Char, scala.Char, OP]
//    protected[twoface] def create[T](value : scala.Char) = new _Char[T](value)
//  }
//
  @bundle
  object Builder {
    final class Macro(val c: whitebox.Context) extends GeneralMacros {
      def binOp[OP](r : c.Tree)(implicit op : c.WeakTypeTag[OP]) : c.Tree =
        TwoFaceMaterializer[OP].binOp(c.prefix.tree, r)
    }
  }

  trait Int extends Any with TwoFaceAny[scala.Int] {
    def +  (r : Int) : Int = macro Builder.Macro.binOp[OpId.+]

//    def == [R <: XChar](r : R)(
//      implicit tfo : Boolean.Return[T == R]
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Char)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XInt](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Int)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XLong](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Long)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XFloat](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Float)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XDouble](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Double)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//      di9 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//
//    def +  [R](r : Char[R])(implicit tfo : Int.Return[T + R])        = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Char[R])(implicit tfo : Int.Return[T - R])        = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Char[R])(implicit tfo : Int.Return[T * R])        = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Char[R])(implicit tfo : Int.Return[T / R])        = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Char[R])(implicit tfo : Int.Return[T % R])        = tfo(this.getValue %  r.getValue)
//    def == [R](r : Char[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Char[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Char[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Char[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Char[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Char[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Int[R])(implicit tfo : Int.Return[T + R])         = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Int[R])(implicit tfo : Int.Return[T - R])         = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Int[R])(implicit tfo : Int.Return[T * R])         = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Int[R])(implicit tfo : Int.Return[T / R])         = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Int[R])(implicit tfo : Int.Return[T % R])         = tfo(this.getValue %  r.getValue)
//    def == [R](r : Int[R])(implicit tfo : Boolean.Return[T == R])    = tfo(this.getValue == r.getValue)
//    def != [R](r : Int[R])(implicit tfo : Boolean.Return[T != R])    = tfo(this.getValue != r.getValue)
//    def <  [R](r : Int[R])(implicit tfo : Boolean.Return[T <  R])    = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Int[R])(implicit tfo : Boolean.Return[T >  R])    = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Int[R])(implicit tfo : Boolean.Return[T <= R])    = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Int[R])(implicit tfo : Boolean.Return[T >= R])    = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Long[R])(implicit tfo : Long.Return[T + R])       = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Long[R])(implicit tfo : Long.Return[T - R])       = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Long[R])(implicit tfo : Long.Return[T * R])       = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Long[R])(implicit tfo : Long.Return[T / R])       = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Long[R])(implicit tfo : Long.Return[T % R])       = tfo(this.getValue %  r.getValue)
//    def == [R](r : Long[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Long[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Long[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Long[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Long[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Long[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Float[R])(implicit tfo : Float.Return[T + R])     = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Float[R])(implicit tfo : Float.Return[T - R])     = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Float[R])(implicit tfo : Float.Return[T * R])     = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Float[R])(implicit tfo : Float.Return[T / R])     = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Float[R])(implicit tfo : Float.Return[T % R])     = tfo(this.getValue %  r.getValue)
//    def == [R](r : Float[R])(implicit tfo : Boolean.Return[T == R])  = tfo(this.getValue == r.getValue)
//    def != [R](r : Float[R])(implicit tfo : Boolean.Return[T != R])  = tfo(this.getValue != r.getValue)
//    def <  [R](r : Float[R])(implicit tfo : Boolean.Return[T <  R])  = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Float[R])(implicit tfo : Boolean.Return[T >  R])  = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Float[R])(implicit tfo : Boolean.Return[T <= R])  = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Float[R])(implicit tfo : Boolean.Return[T >= R])  = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Double[R])(implicit tfo : Double.Return[T + R])   = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Double[R])(implicit tfo : Double.Return[T - R])   = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Double[R])(implicit tfo : Double.Return[T * R])   = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Double[R])(implicit tfo : Double.Return[T / R])   = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Double[R])(implicit tfo : Double.Return[T % R])   = tfo(this.getValue %  r.getValue)
//    def == [R](r : Double[R])(implicit tfo : Boolean.Return[T == R]) = tfo(this.getValue == r.getValue)
//    def != [R](r : Double[R])(implicit tfo : Boolean.Return[T != R]) = tfo(this.getValue != r.getValue)
//    def <  [R](r : Double[R])(implicit tfo : Boolean.Return[T <  R]) = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Double[R])(implicit tfo : Boolean.Return[T >  R]) = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Double[R])(implicit tfo : Boolean.Return[T <= R]) = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Double[R])(implicit tfo : Boolean.Return[T >= R]) = tfo(this.getValue >= r.getValue)
//    def min [R](r : Int[R])(implicit tfo : Int.Return[T Min R])      = tfo(this.getValue min r.getValue)
//    def max [R](r : Int[R])(implicit tfo : Int.Return[T Max R])      = tfo(this.getValue max r.getValue)
//    def unary_-            (implicit tfo : Int.Return[Negate[T]])    = tfo(-this.getValue)
//    def toChar(implicit tfo : Char.Return[ToChar[T]])                = tfo(this.getValue.toChar)
//    def toLong(implicit tfo : Long.Return[ToLong[T]])                = tfo(this.getValue.toLong)
//    def toFloat(implicit tfo : Float.Return[ToFloat[T]])             = tfo(this.getValue.toFloat)
//    def toDouble(implicit tfo : Double.Return[ToDouble[T]])          = tfo(this.getValue.toDouble)
//    def toString(implicit tfo : String.Return[ToString[T]])          = tfo(this.getValue.toString)
  }
  final class _Int[T](val value : scala.Int) extends AnyVal with TwoFaceAny.Int {
    type T0 = T
    @inline def getValue : scala.Int = value
  }
  implicit object Int extends TwoFaceAny.Builder[Int, scala.Int] {
//    type Return[OP] = TwoFaceOp[Int, scala.Int, OP]
    protected[twoface] def create[T](value : scala.Int) = new _Int[T](value)
//    def numberOfLeadingZeros[T](t : Int[T])(implicit tfo : Int.Return[NumberOfLeadingZeros[T]]) =
//      tfo(Integer.numberOfLeadingZeros(t.getValue))
  }
//
//  trait Long[T] extends Any with TwoFaceAny[scala.Long, T] {
//    def == [R <: XChar](r : R)(
//      implicit tfo : Boolean.Return[T == R]
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Char)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XInt](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Int)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XLong](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Long)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XFloat](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Float)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XDouble](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Double)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//      di9 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//
//    def +  [R](r : Char[R])(implicit tfo : Long.Return[T + R])       = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Char[R])(implicit tfo : Long.Return[T - R])       = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Char[R])(implicit tfo : Long.Return[T * R])       = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Char[R])(implicit tfo : Long.Return[T / R])       = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Char[R])(implicit tfo : Long.Return[T % R])       = tfo(this.getValue %  r.getValue)
//    def == [R](r : Char[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Char[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Char[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Char[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Char[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Char[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Int[R])(implicit tfo : Long.Return[T + R])        = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Int[R])(implicit tfo : Long.Return[T - R])        = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Int[R])(implicit tfo : Long.Return[T * R])        = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Int[R])(implicit tfo : Long.Return[T / R])        = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Int[R])(implicit tfo : Long.Return[T % R])        = tfo(this.getValue %  r.getValue)
//    def == [R](r : Int[R])(implicit tfo : Boolean.Return[T == R])    = tfo(this.getValue == r.getValue)
//    def != [R](r : Int[R])(implicit tfo : Boolean.Return[T != R])    = tfo(this.getValue != r.getValue)
//    def <  [R](r : Int[R])(implicit tfo : Boolean.Return[T <  R])    = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Int[R])(implicit tfo : Boolean.Return[T >  R])    = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Int[R])(implicit tfo : Boolean.Return[T <= R])    = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Int[R])(implicit tfo : Boolean.Return[T >= R])    = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Long[R])(implicit tfo : Long.Return[T + R])       = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Long[R])(implicit tfo : Long.Return[T - R])       = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Long[R])(implicit tfo : Long.Return[T * R])       = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Long[R])(implicit tfo : Long.Return[T / R])       = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Long[R])(implicit tfo : Long.Return[T % R])       = tfo(this.getValue %  r.getValue)
//    def == [R](r : Long[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Long[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Long[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Long[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Long[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Long[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Float[R])(implicit tfo : Float.Return[T + R])     = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Float[R])(implicit tfo : Float.Return[T - R])     = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Float[R])(implicit tfo : Float.Return[T * R])     = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Float[R])(implicit tfo : Float.Return[T / R])     = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Float[R])(implicit tfo : Float.Return[T % R])     = tfo(this.getValue %  r.getValue)
//    def == [R](r : Float[R])(implicit tfo : Boolean.Return[T == R])  = tfo(this.getValue == r.getValue)
//    def != [R](r : Float[R])(implicit tfo : Boolean.Return[T != R])  = tfo(this.getValue != r.getValue)
//    def <  [R](r : Float[R])(implicit tfo : Boolean.Return[T <  R])  = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Float[R])(implicit tfo : Boolean.Return[T >  R])  = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Float[R])(implicit tfo : Boolean.Return[T <= R])  = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Float[R])(implicit tfo : Boolean.Return[T >= R])  = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Double[R])(implicit tfo : Double.Return[T + R])   = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Double[R])(implicit tfo : Double.Return[T - R])   = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Double[R])(implicit tfo : Double.Return[T * R])   = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Double[R])(implicit tfo : Double.Return[T / R])   = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Double[R])(implicit tfo : Double.Return[T % R])   = tfo(this.getValue %  r.getValue)
//    def == [R](r : Double[R])(implicit tfo : Boolean.Return[T == R]) = tfo(this.getValue == r.getValue)
//    def != [R](r : Double[R])(implicit tfo : Boolean.Return[T != R]) = tfo(this.getValue != r.getValue)
//    def <  [R](r : Double[R])(implicit tfo : Boolean.Return[T <  R]) = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Double[R])(implicit tfo : Boolean.Return[T >  R]) = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Double[R])(implicit tfo : Boolean.Return[T <= R]) = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Double[R])(implicit tfo : Boolean.Return[T >= R]) = tfo(this.getValue >= r.getValue)
//    def min [R](r : Long[R])(implicit tfo : Long.Return[T Min R])    = tfo(this.getValue min r.getValue)
//    def max [R](r : Long[R])(implicit tfo : Long.Return[T Max R])    = tfo(this.getValue max r.getValue)
//    def unary_-            (implicit tfo : Long.Return[Negate[T]])   = tfo(-this.getValue)
//    def toChar(implicit tfo : Char.Return[ToChar[T]])                = tfo(this.getValue.toChar)
//    def toInt(implicit tfo : Int.Return[ToInt[T]])                   = tfo(this.getValue.toInt)
//    def toFloat(implicit tfo : Float.Return[ToFloat[T]])             = tfo(this.getValue.toFloat)
//    def toDouble(implicit tfo : Double.Return[ToDouble[T]])          = tfo(this.getValue.toDouble)
//    def toString(implicit tfo : String.Return[ToString[T]])          = tfo(this.getValue.toString)
//  }
//  final class _Long[T](val value : scala.Long) extends AnyVal with TwoFaceAny.Long[T] {
//    @inline def getValue : scala.Long = value
//  }
//  implicit object Long extends TwoFaceAny.Builder[Long, scala.Long] {
//    type Return[OP] = TwoFaceOp[Long, scala.Long, OP]
//    protected[twoface] def create[T](value : scala.Long) = new _Long[T](value)
//    def numberOfLeadingZeros[T](t : Long[T])(implicit tfo : Int.Return[NumberOfLeadingZeros[T]]) =
//      tfo(java.lang.Long.numberOfLeadingZeros(t.getValue))
//  }
//
//  trait Float[T] extends Any with TwoFaceAny[scala.Float, T] {
//    def == [R <: XChar](r : R)(
//      implicit tfo : Boolean.Return[T == R]
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Char)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XInt](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Int)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XLong](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Long)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XFloat](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Float)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XDouble](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Double)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//      di9 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//
//    def +  [R](r : Char[R])(implicit tfo : Float.Return[T + R])      = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Char[R])(implicit tfo : Float.Return[T - R])      = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Char[R])(implicit tfo : Float.Return[T * R])      = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Char[R])(implicit tfo : Float.Return[T / R])      = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Char[R])(implicit tfo : Float.Return[T % R])      = tfo(this.getValue %  r.getValue)
//    def == [R](r : Char[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Char[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Char[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Char[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Char[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Char[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Int[R])(implicit tfo : Float.Return[T + R])       = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Int[R])(implicit tfo : Float.Return[T - R])       = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Int[R])(implicit tfo : Float.Return[T * R])       = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Int[R])(implicit tfo : Float.Return[T / R])       = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Int[R])(implicit tfo : Float.Return[T % R])       = tfo(this.getValue %  r.getValue)
//    def == [R](r : Int[R])(implicit tfo : Boolean.Return[T == R])    = tfo(this.getValue == r.getValue)
//    def != [R](r : Int[R])(implicit tfo : Boolean.Return[T != R])    = tfo(this.getValue != r.getValue)
//    def <  [R](r : Int[R])(implicit tfo : Boolean.Return[T <  R])    = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Int[R])(implicit tfo : Boolean.Return[T >  R])    = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Int[R])(implicit tfo : Boolean.Return[T <= R])    = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Int[R])(implicit tfo : Boolean.Return[T >= R])    = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Long[R])(implicit tfo : Float.Return[T + R])      = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Long[R])(implicit tfo : Float.Return[T - R])      = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Long[R])(implicit tfo : Float.Return[T * R])      = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Long[R])(implicit tfo : Float.Return[T / R])      = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Long[R])(implicit tfo : Float.Return[T % R])      = tfo(this.getValue %  r.getValue)
//    def == [R](r : Long[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Long[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Long[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Long[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Long[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Long[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Float[R])(implicit tfo : Float.Return[T + R])     = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Float[R])(implicit tfo : Float.Return[T - R])     = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Float[R])(implicit tfo : Float.Return[T * R])     = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Float[R])(implicit tfo : Float.Return[T / R])     = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Float[R])(implicit tfo : Float.Return[T % R])     = tfo(this.getValue %  r.getValue)
//    def == [R](r : Float[R])(implicit tfo : Boolean.Return[T == R])  = tfo(this.getValue == r.getValue)
//    def != [R](r : Float[R])(implicit tfo : Boolean.Return[T != R])  = tfo(this.getValue != r.getValue)
//    def <  [R](r : Float[R])(implicit tfo : Boolean.Return[T <  R])  = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Float[R])(implicit tfo : Boolean.Return[T >  R])  = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Float[R])(implicit tfo : Boolean.Return[T <= R])  = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Float[R])(implicit tfo : Boolean.Return[T >= R])  = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Double[R])(implicit tfo : Double.Return[T + R])   = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Double[R])(implicit tfo : Double.Return[T - R])   = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Double[R])(implicit tfo : Double.Return[T * R])   = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Double[R])(implicit tfo : Double.Return[T / R])   = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Double[R])(implicit tfo : Double.Return[T % R])   = tfo(this.getValue %  r.getValue)
//    def == [R](r : Double[R])(implicit tfo : Boolean.Return[T == R]) = tfo(this.getValue == r.getValue)
//    def != [R](r : Double[R])(implicit tfo : Boolean.Return[T != R]) = tfo(this.getValue != r.getValue)
//    def <  [R](r : Double[R])(implicit tfo : Boolean.Return[T <  R]) = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Double[R])(implicit tfo : Boolean.Return[T >  R]) = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Double[R])(implicit tfo : Boolean.Return[T <= R]) = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Double[R])(implicit tfo : Boolean.Return[T >= R]) = tfo(this.getValue >= r.getValue)
//    def min [R](r : Float[R])(implicit tfo : Float.Return[T Min R])  = tfo(this.getValue min r.getValue)
//    def max [R](r : Float[R])(implicit tfo : Float.Return[T Max R])  = tfo(this.getValue max r.getValue)
//    def unary_-            (implicit tfo : Float.Return[Negate[T]])  = tfo(-this.getValue)
//    def toChar(implicit tfo : Char.Return[ToChar[T]])                = tfo(this.getValue.toChar)
//    def toInt(implicit tfo : Int.Return[ToInt[T]])                   = tfo(this.getValue.toInt)
//    def toLong(implicit tfo : Long.Return[ToLong[T]])                = tfo(this.getValue.toLong)
//    def toDouble(implicit tfo : Double.Return[ToDouble[T]])          = tfo(this.getValue.toDouble)
//    def toString(implicit tfo : String.Return[ToString[T]])          = tfo(this.getValue.toString)
//  }
//  final class _Float[T](val value : scala.Float) extends AnyVal with TwoFaceAny.Float[T] {
//    @inline def getValue : scala.Float = value
//  }
//  implicit object Float extends TwoFaceAny.Builder[Float, scala.Float] {
//    type Return[OP] = TwoFaceOp[Float, scala.Float, OP]
//    protected[twoface] def create[T](value : scala.Float) = new _Float[T](value)
//  }
//
//  trait Double[T] extends Any with TwoFaceAny[scala.Double, T] {
//    def == [R <: XChar](r : R)(
//      implicit tfo : Boolean.Return[T == R]
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Char)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XInt](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Int)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def == [R <: XLong](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Long)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XFloat](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Float)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == [R <: XDouble](r : R)(
//      implicit tfo : Boolean.Return[T == R],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Double)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit,
//      di2 : DummyImplicit,
//      di3 : DummyImplicit,
//      di4 : DummyImplicit,
//      di5 : DummyImplicit,
//      di6 : DummyImplicit,
//      di7 : DummyImplicit,
//      di8 : DummyImplicit,
//      di9 : DummyImplicit,
//    ) = tfo(this.getValue == r)
//
//    def +  [R](r : Char[R])(implicit tfo : Double.Return[T + R])     = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Char[R])(implicit tfo : Double.Return[T - R])     = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Char[R])(implicit tfo : Double.Return[T * R])     = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Char[R])(implicit tfo : Double.Return[T / R])     = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Char[R])(implicit tfo : Double.Return[T % R])     = tfo(this.getValue %  r.getValue)
//    def == [R](r : Char[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Char[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Char[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Char[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Char[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Char[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Int[R])(implicit tfo : Double.Return[T + R])      = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Int[R])(implicit tfo : Double.Return[T - R])      = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Int[R])(implicit tfo : Double.Return[T * R])      = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Int[R])(implicit tfo : Double.Return[T / R])      = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Int[R])(implicit tfo : Double.Return[T % R])      = tfo(this.getValue %  r.getValue)
//    def == [R](r : Int[R])(implicit tfo : Boolean.Return[T == R])    = tfo(this.getValue == r.getValue)
//    def != [R](r : Int[R])(implicit tfo : Boolean.Return[T != R])    = tfo(this.getValue != r.getValue)
//    def <  [R](r : Int[R])(implicit tfo : Boolean.Return[T <  R])    = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Int[R])(implicit tfo : Boolean.Return[T >  R])    = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Int[R])(implicit tfo : Boolean.Return[T <= R])    = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Int[R])(implicit tfo : Boolean.Return[T >= R])    = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Long[R])(implicit tfo : Double.Return[T + R])     = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Long[R])(implicit tfo : Double.Return[T - R])     = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Long[R])(implicit tfo : Double.Return[T * R])     = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Long[R])(implicit tfo : Double.Return[T / R])     = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Long[R])(implicit tfo : Double.Return[T % R])     = tfo(this.getValue %  r.getValue)
//    def == [R](r : Long[R])(implicit tfo : Boolean.Return[T == R])   = tfo(this.getValue == r.getValue)
//    def != [R](r : Long[R])(implicit tfo : Boolean.Return[T != R])   = tfo(this.getValue != r.getValue)
//    def <  [R](r : Long[R])(implicit tfo : Boolean.Return[T <  R])   = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Long[R])(implicit tfo : Boolean.Return[T >  R])   = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Long[R])(implicit tfo : Boolean.Return[T <= R])   = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Long[R])(implicit tfo : Boolean.Return[T >= R])   = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Float[R])(implicit tfo : Double.Return[T + R])    = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Float[R])(implicit tfo : Double.Return[T - R])    = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Float[R])(implicit tfo : Double.Return[T * R])    = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Float[R])(implicit tfo : Double.Return[T / R])    = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Float[R])(implicit tfo : Double.Return[T % R])    = tfo(this.getValue %  r.getValue)
//    def == [R](r : Float[R])(implicit tfo : Boolean.Return[T == R])  = tfo(this.getValue == r.getValue)
//    def != [R](r : Float[R])(implicit tfo : Boolean.Return[T != R])  = tfo(this.getValue != r.getValue)
//    def <  [R](r : Float[R])(implicit tfo : Boolean.Return[T <  R])  = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Float[R])(implicit tfo : Boolean.Return[T >  R])  = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Float[R])(implicit tfo : Boolean.Return[T <= R])  = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Float[R])(implicit tfo : Boolean.Return[T >= R])  = tfo(this.getValue >= r.getValue)
//    def +  [R](r : Double[R])(implicit tfo : Double.Return[T + R])   = tfo(this.getValue +  r.getValue)
//    def -  [R](r : Double[R])(implicit tfo : Double.Return[T - R])   = tfo(this.getValue -  r.getValue)
//    def *  [R](r : Double[R])(implicit tfo : Double.Return[T * R])   = tfo(this.getValue *  r.getValue)
//    def /  [R](r : Double[R])(implicit tfo : Double.Return[T / R])   = tfo(this.getValue /  r.getValue)
//    def %  [R](r : Double[R])(implicit tfo : Double.Return[T % R])   = tfo(this.getValue %  r.getValue)
//    def == [R](r : Double[R])(implicit tfo : Boolean.Return[T == R]) = tfo(this.getValue == r.getValue)
//    def != [R](r : Double[R])(implicit tfo : Boolean.Return[T != R]) = tfo(this.getValue != r.getValue)
//    def <  [R](r : Double[R])(implicit tfo : Boolean.Return[T <  R]) = tfo(this.getValue <  r.getValue)
//    def >  [R](r : Double[R])(implicit tfo : Boolean.Return[T >  R]) = tfo(this.getValue >  r.getValue)
//    def <= [R](r : Double[R])(implicit tfo : Boolean.Return[T <= R]) = tfo(this.getValue <= r.getValue)
//    def >= [R](r : Double[R])(implicit tfo : Boolean.Return[T >= R]) = tfo(this.getValue >= r.getValue)
//    def min [R](r : Double[R])(implicit tfo : Double.Return[T Min R])= tfo(this.getValue min r.getValue)
//    def max [R](r : Double[R])(implicit tfo : Double.Return[T Max R])= tfo(this.getValue max r.getValue)
//    def unary_-            (implicit tfo : Double.Return[Negate[T]]) = tfo(-this.getValue)
//    def toChar(implicit tfo : Char.Return[ToChar[T]])                = tfo(this.getValue.toChar)
//    def toInt(implicit tfo : Int.Return[ToInt[T]])                   = tfo(this.getValue.toInt)
//    def toLong(implicit tfo : Long.Return[ToLong[T]])                = tfo(this.getValue.toLong)
//    def toFloat(implicit tfo : Float.Return[ToFloat[T]])             = tfo(this.getValue.toFloat)
//    def toString(implicit tfo : String.Return[ToString[T]])          = tfo(this.getValue.toString)
//  }
//  final class _Double[T](val value : scala.Double) extends AnyVal with TwoFaceAny.Double[T] {
//    @inline def getValue : scala.Double = value
//  }
//  implicit object Double extends TwoFaceAny.Builder[Double, scala.Double] {
//    type Return[OP] = TwoFaceOp[Double, scala.Double, OP]
//    protected[twoface] def create[T](value : scala.Double) = new _Double[T](value)
//  }
//
//  trait String[T] extends Any with TwoFaceAny[java.lang.String, T] {
//    def +  [R](r : String[R])(implicit tfo : String.Return[T + R])   = tfo(this.getValue +  r.getValue)
//    def == [R](r : String[R])(implicit tfo : Boolean.Return[T == R]) = tfo(this.getValue == r.getValue)
//    def != [R](r : String[R])(implicit tfo : Boolean.Return[T != R]) = tfo(this.getValue != r.getValue)
//    def == [R <: XString](r : R)(
//      implicit tfo : Boolean.Return[T == R]
//    ) = tfo(this.getValue == r)
//    def == (r : java.lang.String)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def reverse(implicit tfo : String.Return[Reverse[T]])            = tfo(this.getValue.reverse)
//    def substring[R](r : Int[R])(implicit tfo : String.Return[Substring[T,R]])= tfo(this.getValue substring r.getValue)
//    def length(implicit tfo : Int.Return[Length[T]])                 = tfo(this.getValue.length)
//    def charAt[R](r : Int[R])(implicit tfo : Char.Return[CharAt[T,R]])= tfo(this.getValue charAt r.getValue)
//    def toInt(implicit tfo : Int.Return[ToInt[T]])                   = tfo(this.getValue.toInt)
//    def toLong(implicit tfo : Long.Return[ToLong[T]])                = tfo(this.getValue.toLong)
//    def toFloat(implicit tfo : Float.Return[ToFloat[T]])             = tfo(this.getValue.toFloat)
//    def toDouble(implicit tfo : Double.Return[ToDouble[T]])          = tfo(this.getValue.toDouble)
//  }
//  final class _String[T](val value : java.lang.String) extends AnyVal with TwoFaceAny.String[T] {
//    @inline def getValue : java.lang.String = value
//  }
//  implicit object String extends TwoFaceAny.Builder[String, java.lang.String] {
//    type Return[OP] = TwoFaceOp[String, java.lang.String, OP]
//    protected[twoface] def create[T](value : java.lang.String) = new _String[T](value)
//  }
//
//  trait Boolean[T] extends Any with TwoFaceAny[scala.Boolean, T] {
//    def == [R](r : Boolean[R])(implicit tfo : Boolean.Return[T == R])= tfo(this.getValue == r.getValue)
//    def != [R](r : Boolean[R])(implicit tfo : Boolean.Return[T != R])= tfo(this.getValue != r.getValue)
//    def && [R](r : Boolean[R])(implicit tfo : Boolean.Return[T && R])= tfo(this.getValue && r.getValue)
//    def || [R](r : Boolean[R])(implicit tfo : Boolean.Return[T || R])= tfo(this.getValue || r.getValue)
//    def unary_!(implicit tfo : Boolean.Return[![T]])= tfo(!this.getValue)
//    def == [R <: XBoolean](r : R)(
//      implicit tfo : Boolean.Return[T == R]
//    ) = tfo(this.getValue == r)
//    def == (r : scala.Boolean)(
//      implicit tfo : Boolean.Return[scala.Boolean],
//      di1 : DummyImplicit
//    ) = tfo(this.getValue == r)
//    def toString(implicit tfo : String.Return[ToString[T]])          = tfo(this.getValue.toString)
//  }
//  final class _Boolean[T](val value : scala.Boolean) extends AnyVal with TwoFaceAny.Boolean[T] {
//    @inline def getValue : scala.Boolean = value
//  }
//  implicit object Boolean extends TwoFaceAny.Builder[Boolean, scala.Boolean] {
//    type Return[OP] = TwoFaceOp[Boolean, scala.Boolean, OP]
//    protected[twoface] def create[T](value : scala.Boolean) = new _Boolean[T](value)
//  }
//
}
