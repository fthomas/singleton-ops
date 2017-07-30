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

  trait Builder[TF[T], Face] {
    type Shell1[Func[_], Arg1, Arg1Wide] =
      Shell.One[Func[Arg1], Func[Arg[1, Arg1]], Arg1, Arg1Wide]
    type Shell2[Func[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] =
      Shell.Two[Func[Arg1, Arg2], Func[Arg[1, Arg1], Arg[2, Arg2]], Arg1, Arg1Wide, Arg2, Arg2Wide]
    type Shell3[Func[_,_,_], Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide] =
      Shell.Three[Func[Arg1, Arg2, Arg3], Func[Arg[1, Arg1], Arg[2, Arg2], Arg[3, Arg3]], Arg1, Arg1Wide, Arg2, Arg2Wide, Arg3, Arg3Wide]
    protected[singleton] def create[T](value : Face) : TF[T]
    implicit def apply[T <: Face with Singleton](value : T)(implicit tfb : Builder[TF, Face])
    : TF[T] =  tfb.create[T](value)
    implicit def apply(value : Face)(implicit tfb : Builder[TF, Face], di: DummyImplicit)
    : TF[Face] = tfb.create[Face](value)
    implicit def apply[T](implicit id : ValueOf[T], tfb : Builder[TF, Face], di: DummyImplicit, di2 : DummyImplicit)
    : TF[T] = tfb.create[T](valueOf[T].asInstanceOf[Face])
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
  implicit object Char extends TwoFaceAny.Builder[Char, scala.Char] {
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
  implicit object Int extends TwoFaceAny.Builder[Int, scala.Int] {
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

    def unary_-(implicit tfs : Int.Shell1[Negate, T, scala.Long]) = tfs(this)
    def toChar(implicit tfs : Char.Shell1[ToChar, T, scala.Long]) = tfs(this)
    def toInt(implicit tfs : Int.Shell1[ToInt, T, scala.Long]) = tfs(this)
    def toFloat(implicit tfs : Float.Shell1[ToFloat, T, scala.Long]) = tfs(this)
    def toDouble(implicit tfs : Double.Shell1[ToDouble, T, scala.Long]) = tfs(this)
    def toStringTF(implicit tfs : String.Shell1[ToString, T, scala.Long]) = tfs(this)
  }

  final class _Long[T](val value : scala.Long) extends AnyVal with TwoFaceAny.Long[T] {
    @inline def getValue : scala.Long = value
  }
  implicit object Long extends TwoFaceAny.Builder[Long, scala.Long] {
    protected[singleton] def create[T](value : scala.Long) = new _Long[T](value)
    def numberOfLeadingZeros[T](t : Long[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, scala.Long]) = tfs(t)
  }

  trait Float[T] extends Any with TwoFaceAny[scala.Float, T] {
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
  }
  final class _Float[T](val value : scala.Float) extends AnyVal with TwoFaceAny.Float[T] {
    @inline def getValue : scala.Float = value
  }
  implicit object Float extends TwoFaceAny.Builder[Float, scala.Float] {
    protected[singleton] def create[T](value : scala.Float) = new _Float[T](value)
  }

  trait Double[T] extends Any with TwoFaceAny[scala.Double, T] {
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
  }
  final class _Double[T](val value : scala.Double) extends AnyVal with TwoFaceAny.Double[T] {
    @inline def getValue : scala.Double = value
  }
  implicit object Double extends TwoFaceAny.Builder[Double, scala.Double] {
    protected[singleton] def create[T](value : scala.Double) = new _Double[T](value)
  }

  trait String[T] extends Any with TwoFaceAny[java.lang.String, T] {
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
  }
  final class _String[T](val value : java.lang.String) extends AnyVal with TwoFaceAny.String[T] {
    @inline def getValue : java.lang.String = value
  }
  implicit object String extends TwoFaceAny.Builder[String, java.lang.String] {
    protected[singleton] def create[T](value : java.lang.String) = new _String[T](value)
  }

  trait Boolean[T] extends Any with TwoFaceAny[scala.Boolean, T] {
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
  }
  final class _Boolean[T](val value : scala.Boolean) extends AnyVal with TwoFaceAny.Boolean[T] {
    @inline def getValue : scala.Boolean = value
  }
  implicit object Boolean extends TwoFaceAny.Builder[Boolean, scala.Boolean] {
    protected[singleton] def create[T](value : scala.Boolean) = new _Boolean[T](value)
  }

}
