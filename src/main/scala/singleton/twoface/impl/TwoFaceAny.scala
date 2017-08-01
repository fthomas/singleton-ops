package singleton.twoface.impl

import singleton.ops._

trait TwoFaceAny[Face, T] extends Any {
  type ShellArg = Tuple2[T, Face]
  def isLiteral(implicit rt : RunTime[T]) : scala.Boolean = !rt
  @inline def getValue : Face
  override def toString = getValue.toString
}

object TwoFaceAny {
  @inline implicit def fromTwoFaceUnsafe[Face, T](tf : TwoFaceAny[Face, T]) : Face = tf.getValue
  @inline implicit def fromTwoFaceSafe[Face, T <: Face with Singleton](tf : TwoFaceAny[Face, T])
                                                                      (implicit sc: ValueOf[T]) : T {} = valueOf[T]

  trait Builder[TF[T], Face, Shl1[_,_,_,_], Shl2[_,_,_,_,_,_], Shl3[_,_,_,_,_,_,_,_]] {
    type Shell1[Func[_], Arg1, Arg1Wide] =
      Shl1[Func[Arg1],
           Func[Arg[1, Arg1, Arg1Wide]],
           Arg1, Arg1Wide]
    type Shell2[Func[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shl2[Func[Arg1, Arg2],
           Func[Arg[1, Arg1, Arg1Wide], Arg[2, Arg2, Arg2Wide]],
           Arg1, Arg1Wide, Arg2, Arg2Wide]
    type Shell3[Func[_,_,_], Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide] =
      Shl3[Func[Arg1, Arg2, Arg3],
           Func[Arg[1, Arg1, Arg1Wide], Arg[2, Arg2, Arg2Wide], Arg[3, Arg3, Arg3Wide]],
           Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]
    protected[singleton] def create[T](value : Face) : TF[T]
    implicit def apply[T <: Face with Singleton](value : T)(implicit tfb : Builder[TF, Face, Shl1, Shl2, Shl3])
    : TF[T] =  tfb.create[T](value)
    implicit def apply(value : Face)(implicit tfb : Builder[TF, Face, Shl1, Shl2, Shl3], di: DummyImplicit)
    : TF[Face] = tfb.create[Face](value)
    implicit def apply[T](implicit id : Id[T], tfb : Builder[TF, Face, Shl1, Shl2, Shl3], di: DummyImplicit, di2 : DummyImplicit)
    : TF[T] = tfb.create[T](id.valueWide.asInstanceOf[Face])
  }

  trait Char[T] extends Any with TwoFaceAny[scala.Char, T] {
    def == (r : scala.Char) = Boolean(this.getValue == r)
    def == (r : scala.Int) = Boolean(this.getValue == r)
    def == (r : scala.Long) = Boolean(this.getValue == r)
    def == (r : scala.Float) = Boolean(this.getValue == r)
    def == (r : scala.Double) = Boolean(this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Char]
    ) = tfs(this, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this, r)


    def +  [R](r : Char[R])(implicit tfs : Int.Shell2[+, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def +  [R](r : Int[R])(implicit tfs : Int.Shell2[+, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def -  [R](r : Char[R])(implicit tfs : Int.Shell2[-, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def -  [R](r : Int[R])(implicit tfs : Int.Shell2[-, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def *  [R](r : Char[R])(implicit tfs : Int.Shell2[*, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def *  [R](r : Int[R])(implicit tfs : Int.Shell2[*, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def /  [R](r : Char[R])(implicit tfs : Int.Shell2[/, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def /  [R](r : Int[R])(implicit tfs : Int.Shell2[/, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def %  [R](r : Char[R])(implicit tfs : Int.Shell2[%, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def %  [R](r : Int[R])(implicit tfs : Int.Shell2[%, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Char, R, scala.Double]) = tfs(this, r)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Char]) = tfs(this, r)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Int]) = tfs(this, r)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Long]) = tfs(this, r)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Float]) = tfs(this, r)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Char, R, scala.Double]) = tfs(this, r)

    def unary_-(implicit tfs : Int.Shell1[Negate, T, scala.Char]) = tfs(this)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Char]) = tfs(this)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Char]) = tfs(this)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Char]) = tfs(this)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Char]) = tfs(this)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Char]) = tfs(this)
  }
  final class _Char[T](val value : scala.Char) extends AnyVal with TwoFaceAny.Char[T] {
    @inline def getValue : scala.Char = value
  }
  implicit object Char extends TwoFaceAny.Builder[Char, scala.Char, Shell.One.Char, Shell.Two.Char, Shell.Three.Char] {
    protected[singleton] def create[T](value : scala.Char) = new _Char[T](value)
  }

  trait Int[T] extends Any with TwoFaceAny[scala.Int, T] {
    def == (r : scala.Char) = Boolean(this.getValue == r)
    def == (r : scala.Int) = Boolean(this.getValue == r)
    def == (r : scala.Long) = Boolean(this.getValue == r)
    def == (r : scala.Float) = Boolean(this.getValue == r)
    def == (r : scala.Double) = Boolean(this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Char]
    ) = tfs(this, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this, r)

    def +  [R](r : Char[R])(implicit tfs : Int.Shell2[+, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def +  [R](r : Int[R])(implicit tfs : Int.Shell2[+, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def -  [R](r : Char[R])(implicit tfs : Int.Shell2[-, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def -  [R](r : Int[R])(implicit tfs : Int.Shell2[-, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def *  [R](r : Char[R])(implicit tfs : Int.Shell2[*, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def *  [R](r : Int[R])(implicit tfs : Int.Shell2[*, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def /  [R](r : Char[R])(implicit tfs : Int.Shell2[/, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def /  [R](r : Int[R])(implicit tfs : Int.Shell2[/, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def %  [R](r : Char[R])(implicit tfs : Int.Shell2[%, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def %  [R](r : Int[R])(implicit tfs : Int.Shell2[%, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Int, R, scala.Double]) = tfs(this, r)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Char]) = tfs(this, r)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Int]) = tfs(this, r)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Long]) = tfs(this, r)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Float]) = tfs(this, r)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Int, R, scala.Double]) = tfs(this, r)

    def unary_-(implicit tfs : Int.Shell1[Negate, T, scala.Int]) = tfs(this)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Int]) = tfs(this)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Int]) = tfs(this)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Int]) = tfs(this)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Int]) = tfs(this)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Int]) = tfs(this)
  }
  final class _Int[T](val value : scala.Int) extends AnyVal with TwoFaceAny.Int[T] {
    @inline def getValue : scala.Int = value
  }
  implicit object Int extends TwoFaceAny.Builder[Int, scala.Int, Shell.One.Int, Shell.Two.Int, Shell.Three.Int] {
    protected[singleton] def create[T](value : scala.Int) : Int[T] = new _Int[T](value)
    def numberOfLeadingZeros[T](t : Int[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, scala.Int]) = tfs(t)
  }

  trait Long[T] extends Any with TwoFaceAny[scala.Long, T] {
    def == (r : scala.Char) = Boolean(this.getValue == r)
    def == (r : scala.Int) = Boolean(this.getValue == r)
    def == (r : scala.Long) = Boolean(this.getValue == r)
    def == (r : scala.Float) = Boolean(this.getValue == r)
    def == (r : scala.Double) = Boolean(this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Char]
    ) = tfs(this, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this, r)

    def +  [R](r : Char[R])(implicit tfs : Long.Shell2[+, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def +  [R](r : Int[R])(implicit tfs : Long.Shell2[+, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def +  [R](r : Long[R])(implicit tfs : Long.Shell2[+, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def -  [R](r : Char[R])(implicit tfs : Long.Shell2[-, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def -  [R](r : Int[R])(implicit tfs : Long.Shell2[-, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def -  [R](r : Long[R])(implicit tfs : Long.Shell2[-, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def *  [R](r : Char[R])(implicit tfs : Long.Shell2[*, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def *  [R](r : Int[R])(implicit tfs : Long.Shell2[*, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def *  [R](r : Long[R])(implicit tfs : Long.Shell2[*, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def /  [R](r : Char[R])(implicit tfs : Long.Shell2[/, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def /  [R](r : Int[R])(implicit tfs : Long.Shell2[/, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def /  [R](r : Long[R])(implicit tfs : Long.Shell2[/, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def %  [R](r : Char[R])(implicit tfs : Long.Shell2[%, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def %  [R](r : Int[R])(implicit tfs : Long.Shell2[%, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def %  [R](r : Long[R])(implicit tfs : Long.Shell2[%, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Long, R, scala.Double]) = tfs(this, r)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Char]) = tfs(this, r)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Int]) = tfs(this, r)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Long]) = tfs(this, r)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Float]) = tfs(this, r)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Long, R, scala.Double]) = tfs(this, r)

    def unary_-(implicit tfs : Long.Shell1[Negate, T, scala.Long]) = tfs(this)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Long]) = tfs(this)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Long]) = tfs(this)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Long]) = tfs(this)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Long]) = tfs(this)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Long]) = tfs(this)
  }

  final class _Long[T](val value : scala.Long) extends AnyVal with TwoFaceAny.Long[T] {
    @inline def getValue : scala.Long = value
  }
  implicit object Long extends TwoFaceAny.Builder[Long, scala.Long, Shell.One.Long, Shell.Two.Long, Shell.Three.Long] {
    protected[singleton] def create[T](value : scala.Long) = new _Long[T](value)
    def numberOfLeadingZeros[T](t : Long[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, scala.Long]) = tfs(t)
  }

  trait Float[T] extends Any with TwoFaceAny[scala.Float, T] {
    def == (r : scala.Char) = Boolean(this.getValue == r)
    def == (r : scala.Int) = Boolean(this.getValue == r)
    def == (r : scala.Long) = Boolean(this.getValue == r)
    def == (r : scala.Float) = Boolean(this.getValue == r)
    def == (r : scala.Double) = Boolean(this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Char]
    ) = tfs(this, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this, r)

    def +  [R](r : Char[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def +  [R](r : Int[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def +  [R](r : Long[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def +  [R](r : Float[R])(implicit tfs : Float.Shell2[+, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def -  [R](r : Char[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def -  [R](r : Int[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def -  [R](r : Long[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def -  [R](r : Float[R])(implicit tfs : Float.Shell2[-, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def *  [R](r : Char[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def *  [R](r : Int[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def *  [R](r : Long[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def *  [R](r : Float[R])(implicit tfs : Float.Shell2[*, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def /  [R](r : Char[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def /  [R](r : Int[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def /  [R](r : Long[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def /  [R](r : Float[R])(implicit tfs : Float.Shell2[/, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def %  [R](r : Char[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def %  [R](r : Int[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def %  [R](r : Long[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def %  [R](r : Float[R])(implicit tfs : Float.Shell2[%, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Float, R, scala.Double]) = tfs(this, r)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Char]) = tfs(this, r)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Int]) = tfs(this, r)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Long]) = tfs(this, r)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Float]) = tfs(this, r)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Float, R, scala.Double]) = tfs(this, r)

    def unary_-(implicit tfs : Float.Shell1[Negate, T, scala.Float]) = tfs(this)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Float]) = tfs(this)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Float]) = tfs(this)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Float]) = tfs(this)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Float]) = tfs(this)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Float]) = tfs(this)
  }
  final class _Float[T](val value : scala.Float) extends AnyVal with TwoFaceAny.Float[T] {
    @inline def getValue : scala.Float = value
  }
  implicit object Float extends TwoFaceAny.Builder[Float, scala.Float, Shell.One.Float, Shell.Two.Float, Shell.Three.Float] {
    protected[singleton] def create[T](value : scala.Float) = new _Float[T](value)
  }

  trait Double[T] extends Any with TwoFaceAny[scala.Double, T] {
    def == (r : scala.Char) = Boolean(this.getValue == r)
    def == (r : scala.Int) = Boolean(this.getValue == r)
    def == (r : scala.Long) = Boolean(this.getValue == r)
    def == (r : scala.Float) = Boolean(this.getValue == r)
    def == (r : scala.Double) = Boolean(this.getValue == r)
    def == [R <: XChar](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Char]
    ) = tfs(this, r)
    def == [R <: XInt](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Int],
      di1 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XLong](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Long],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XFloat](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Float],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
    ) = tfs(this, r)
    def == [R <: XDouble](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Double],
      di1 : DummyImplicit,
      di2 : DummyImplicit,
      di3 : DummyImplicit,
      di4 : DummyImplicit,
    ) = tfs(this, r)

    def +  [R](r : Char[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def +  [R](r : Int[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def +  [R](r : Long[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def +  [R](r : Float[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def +  [R](r : Double[R])(implicit tfs : Double.Shell2[+, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def -  [R](r : Char[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def -  [R](r : Int[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def -  [R](r : Long[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def -  [R](r : Float[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def -  [R](r : Double[R])(implicit tfs : Double.Shell2[-, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def *  [R](r : Char[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def *  [R](r : Int[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def *  [R](r : Long[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def *  [R](r : Float[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def *  [R](r : Double[R])(implicit tfs : Double.Shell2[*, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def /  [R](r : Char[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def /  [R](r : Int[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def /  [R](r : Long[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def /  [R](r : Float[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def /  [R](r : Double[R])(implicit tfs : Double.Shell2[/, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def %  [R](r : Char[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def %  [R](r : Int[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def %  [R](r : Long[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def %  [R](r : Float[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def %  [R](r : Double[R])(implicit tfs : Double.Shell2[%, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def <  [R](r : Char[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def <  [R](r : Int[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def <  [R](r : Long[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def <  [R](r : Float[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def <  [R](r : Double[R])(implicit tfs : Boolean.Shell2[<, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def >  [R](r : Char[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def >  [R](r : Int[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def >  [R](r : Long[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def >  [R](r : Float[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def >  [R](r : Double[R])(implicit tfs : Boolean.Shell2[>, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def <= [R](r : Char[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def <= [R](r : Int[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def <= [R](r : Long[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def <= [R](r : Float[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def <= [R](r : Double[R])(implicit tfs : Boolean.Shell2[<=, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def >= [R](r : Char[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def >= [R](r : Int[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def >= [R](r : Long[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def >= [R](r : Float[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def >= [R](r : Double[R])(implicit tfs : Boolean.Shell2[>=, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def == [R](r : Char[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def == [R](r : Int[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def == [R](r : Long[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def == [R](r : Float[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def == [R](r : Double[R])(implicit tfs : Boolean.Shell2[==, T, scala.Double, R, scala.Double]) = tfs(this, r)
    def != [R](r : Char[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Char]) = tfs(this, r)
    def != [R](r : Int[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Int]) = tfs(this, r)
    def != [R](r : Long[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Long]) = tfs(this, r)
    def != [R](r : Float[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Float]) = tfs(this, r)
    def != [R](r : Double[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Double, R, scala.Double]) = tfs(this, r)

    def unary_-(implicit tfs : Double.Shell1[Negate, T, scala.Double]) = tfs(this)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Double]) = tfs(this)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Double]) = tfs(this)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, scala.Double]) = tfs(this)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Double]) = tfs(this)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Double]) = tfs(this)
  }
  final class _Double[T](val value : scala.Double) extends AnyVal with TwoFaceAny.Double[T] {
    @inline def getValue : scala.Double = value
  }
  implicit object Double extends TwoFaceAny.Builder[Double, scala.Double, Shell.One.Double, Shell.Two.Double, Shell.Three.Double] {
    protected[singleton] def create[T](value : scala.Double) = new _Double[T](value)
  }

  trait String[T] extends Any with TwoFaceAny[java.lang.String, T] {
    def == (r : java.lang.String) = Boolean(this.getValue == r)
    def == [R <: XString](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, java.lang.String, R, java.lang.String]
    ) = tfs(this, r)

    def +  [R](r : String[R])(implicit tfs : String.Shell2[+, T, java.lang.String, R, java.lang.String]) = tfs(this, r)
    def == [R](r : String[R])(implicit tfs : Boolean.Shell2[==, T, java.lang.String, R, java.lang.String]) = tfs(this, r)
    def != [R](r : String[R])(implicit tfs : Boolean.Shell2[!=, T, java.lang.String, R, java.lang.String]) = tfs(this, r)

    def reverse(implicit tfs : String.Shell1[Reverse, T, java.lang.String]) = tfs(this)
    def substring[R](r : Int[R])(implicit tfs : String.Shell2[Substring, T, java.lang.String, R, scala.Int]) = tfs(this, r)
    def length(implicit tfs : Int.Shell1[Length, T, java.lang.String]) = tfs(this)
    def charAt[R](r : Int[R])(implicit tfs : Char.Shell2[CharAt, T, java.lang.String, R, scala.Int]) = tfs(this, r)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, java.lang.String]) = tfs(this)
    def toLong(implicit tfs : Long.Shell1[ToLong, T, java.lang.String]) = tfs(this)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, java.lang.String]) = tfs(this)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, java.lang.String]) = tfs(this)
  }
  final class _String[T](val value : java.lang.String) extends AnyVal with TwoFaceAny.String[T] {
    @inline def getValue : java.lang.String = value
  }
  implicit object String extends TwoFaceAny.Builder[String, java.lang.String, Shell.One.String, Shell.Two.String, Shell.Three.String] {
    protected[singleton] def create[T](value : java.lang.String) = new _String[T](value)
  }

  trait Boolean[T] extends Any with TwoFaceAny[scala.Boolean, T] {
    def == (r : scala.Boolean) = Boolean(this.getValue == r)
    def == [R <: XBoolean](r : R)(
      implicit
      tfs : Boolean.Shell2[==, T, scala.Boolean, R, scala.Boolean]
    ) = tfs(this, r)
    def == [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[==, T, scala.Boolean, R, scala.Boolean]) = tfs(this, r)
    def != [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[!=, T, scala.Boolean, R, scala.Boolean]) = tfs(this, r)
    def && [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[&&, T, scala.Boolean, R, scala.Boolean]) = tfs(this, r)
    def || [R](r : Boolean[R])(implicit tfs : Boolean.Shell2[||, T, scala.Boolean, R, scala.Boolean]) = tfs(this, r)
    def unary_!(implicit tfs : Boolean.Shell1[!, T, scala.Boolean]) = tfs(this)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Boolean]) = tfs(this)
  }
  final class _Boolean[T](val value : scala.Boolean) extends AnyVal with TwoFaceAny.Boolean[T] {
    @inline def getValue : scala.Boolean = value
  }
  implicit object Boolean extends TwoFaceAny.Builder[Boolean, scala.Boolean, Shell.One.Boolean, Shell.Two.Boolean, Shell.Three.Boolean] {
    protected[singleton] def create[T](value : scala.Boolean) = new _Boolean[T](value)
  }

}
