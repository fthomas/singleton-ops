package singleton.twoface.impl

import singleton.ops._
import singleton.ops.impl.{std, HasOut}

trait TwoFaceAny[Face, T] extends Any with HasOut {
  type Out = T
  def isLiteral(implicit rt : RunTime[T]) : std.Boolean = !rt
  @inline def getValue : Face
  override def toString = getValue.toString
}

object TwoFaceAny {
  trait Builder[TF[T], Face, Shl1[_,_,_,_] <: HasOut, Shl2[_,_,_,_,_,_] <: HasOut, Shl3[_,_,_,_,_,_,_,_] <: HasOut] {
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
    def == [R](r : Char[R])(implicit ccs : CaseClassSkipper[Boolean.Shell2[==, T, std.Char, R, std.Char]]) =
      ccs(tfs => tfs(this.getValue, r.getValue), this.getValue.asInstanceOf[Any] == r.getValue.asInstanceOf[Any])
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
  implicit object Char extends TwoFaceAny.Builder[Char, std.Char, Shell.One.Char, Shell.Two.Char, Shell.Three.Char] with OpContainer.Eq1[Char, std.Char] {
    def create[T](value : std.Char) : Char[T] = new _Char[T](value)
    def apply[T <: std.Char with Singleton](implicit value : ValueOf[T], di : DummyImplicit, di2 : DummyImplicit) : Char[T] = create[T](valueOf[T])
    implicit def apply[T <: std.Char with Singleton](value : T) : Char[T] = create[T](value)
    implicit def apply[T <: std.Char](value : T)(implicit di : DummyImplicit) : Char[T] = create[T](value)
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Char[T] = create[T](id.valueWide.asInstanceOf[std.Char])
    implicit def tf2NumSingleton[T <: std.Char with Singleton](tf : Char[T])(implicit value : ValueOf[T]) : T = valueOf[T]
    implicit def tf2NumOp[T](tf : Char[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.OutChar = id.value.asInstanceOf[id.OutChar]
    implicit def tf2NumWide[T](tf : Char[T])(implicit di : DummyImplicit, di2 : DummyImplicit) : std.Char = tf.getValue
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
    def == [R](r : Int[R])(implicit ccs : CaseClassSkipper[Boolean.Shell2[==, T, std.Int, R, std.Int]]) =
      ccs(tfs => tfs(this.getValue, r.getValue), this.getValue.asInstanceOf[Any] == r.getValue.asInstanceOf[Any])
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
  implicit object Int extends TwoFaceAny.Builder[Int, std.Int, Shell.One.Int, Shell.Two.Int, Shell.Three.Int] with OpContainer.Eq1[Int, std.Int] {
    def numberOfLeadingZeros[T](t : Int[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Int]) = tfs(t.getValue)
    def create[T](value : std.Int) : Int[T] = new _Int[T](value)
    def apply[T <: std.Int with Singleton](implicit value : ValueOf[T], di : DummyImplicit, di2 : DummyImplicit) : Int[T] = create[T](valueOf[T])
    implicit def apply[T <: std.Int with Singleton](value : T) : Int[T] = create[T](value)
    implicit def apply[T <: std.Int](value : T)(implicit di : DummyImplicit) : Int[T] = create[T](value)
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Int[T] = create[T](id.valueWide.asInstanceOf[std.Int])
    implicit def tf2NumSingleton[T <: std.Int with Singleton](tf : Int[T])(implicit value : ValueOf[T]) : T = valueOf[T]
    implicit def tf2NumOp[T](tf : Int[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.OutInt = id.value.asInstanceOf[id.OutInt]
    implicit def tf2NumWide[T](tf : Int[T])(implicit di : DummyImplicit, di2 : DummyImplicit) : std.Int = tf.getValue
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
    def == [R](r : Long[R])(implicit ccs : CaseClassSkipper[Boolean.Shell2[==, T, std.Long, R, std.Long]]) =
      ccs(tfs => tfs(this.getValue, r.getValue), this.getValue.asInstanceOf[Any] == r.getValue.asInstanceOf[Any])
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
  implicit object Long extends TwoFaceAny.Builder[Long, std.Long, Shell.One.Long, Shell.Two.Long, Shell.Three.Long] with OpContainer.Eq1[Long, std.Long] {
    def numberOfLeadingZeros[T](t : Long[T])(implicit tfs : Int.Shell1[NumberOfLeadingZeros, T, std.Long]) = tfs(t.getValue)
    def create[T](value : std.Long) : Long[T] = new _Long[T](value)
    def apply[T <: std.Long with Singleton](implicit value : ValueOf[T], di : DummyImplicit, di2 : DummyImplicit) : Long[T] = create[T](valueOf[T])
    implicit def apply[T <: std.Long with Singleton](value : T) : Long[T] = create[T](value)
    implicit def apply[T <: std.Long](value : T)(implicit di : DummyImplicit) : Long[T] = create[T](value)
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Long[T] = create[T](id.valueWide.asInstanceOf[std.Long])
    implicit def tf2NumSingleton[T <: std.Long with Singleton](tf : Long[T])(implicit value : ValueOf[T]) : T = valueOf[T]
    implicit def tf2NumOp[T](tf : Long[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.OutLong = id.value.asInstanceOf[id.OutLong]
    implicit def tf2NumWide[T](tf : Long[T])(implicit di : DummyImplicit, di2 : DummyImplicit) : std.Long = tf.getValue
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
    def == [R](r : Float[R])(implicit ccs : CaseClassSkipper[Boolean.Shell2[==, T, std.Float, R, std.Float]]) =
      ccs(tfs => tfs(this.getValue, r.getValue), this.getValue.asInstanceOf[Any] == r.getValue.asInstanceOf[Any])
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
  implicit object Float extends TwoFaceAny.Builder[Float, std.Float, Shell.One.Float, Shell.Two.Float, Shell.Three.Float] with OpContainer.Eq1[Float, std.Float] {
    def create[T](value : std.Float) : Float[T] = new _Float[T](value)
    def apply[T <: std.Float with Singleton](implicit value : ValueOf[T], di : DummyImplicit, di2 : DummyImplicit) : Float[T] = create[T](valueOf[T])
    implicit def apply[T <: std.Float with Singleton](value : T) : Float[T] = create[T](value)
    implicit def apply[T <: std.Float](value : T)(implicit di : DummyImplicit) : Float[T] = create[T](value)
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Float[T] = create[T](id.valueWide.asInstanceOf[std.Float])
    implicit def tf2NumSingleton[T <: std.Float with Singleton](tf : Float[T])(implicit value : ValueOf[T]) : T = valueOf[T]
    implicit def tf2NumOp[T](tf : Float[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.OutFloat = id.value.asInstanceOf[id.OutFloat]
    implicit def tf2NumWide[T](tf : Float[T])(implicit di : DummyImplicit, di2 : DummyImplicit) : std.Float = tf.getValue
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
    def == [R](r : Double[R])(implicit ccs : CaseClassSkipper[Boolean.Shell2[==, T, std.Double, R, std.Double]]) =
      ccs(tfs => tfs(this.getValue, r.getValue), this.getValue.asInstanceOf[Any] == r.getValue.asInstanceOf[Any])
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
  implicit object Double extends TwoFaceAny.Builder[Double, std.Double, Shell.One.Double, Shell.Two.Double, Shell.Three.Double] with OpContainer.Eq1[Double, std.Double] {
    def create[T](value : std.Double) : Double[T] = new _Double[T](value)
    def apply[T <: std.Double with Singleton](implicit value : ValueOf[T], di : DummyImplicit, di2 : DummyImplicit) : Double[T] = create[T](valueOf[T])
    implicit def apply[T <: std.Double with Singleton](value : T) : Double[T] = create[T](value)
    implicit def apply[T <: std.Double](value : T)(implicit di : DummyImplicit) : Double[T] = create[T](value)
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Double[T] = create[T](id.valueWide.asInstanceOf[std.Double])
    implicit def tf2NumSingleton[T <: std.Double with Singleton](tf : Double[T])(implicit value : ValueOf[T]) : T = valueOf[T]
    implicit def tf2NumOp[T](tf : Double[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.OutDouble = id.value.asInstanceOf[id.OutDouble]
    implicit def tf2NumWide[T](tf : Double[T])(implicit di : DummyImplicit, di2 : DummyImplicit) : std.Double = tf.getValue
  }

  trait String[T] extends Any with TwoFaceAny[std.String, T] {
    def == (r : std.String)(implicit tfs : Boolean.Shell2[==, T, std.String, GetArg0, std.String]) = tfs(this.getValue, r)
    def != (r : std.String)(implicit tfs : Boolean.Shell2[!=, T, std.String, GetArg0, std.String]) = tfs(this.getValue, r)

    def +  [R](r : String[R])(implicit tfs : String.Shell2[+, T, std.String, R, std.String]) = tfs(this.getValue, r.getValue)
    def == [R](r : String[R])(implicit ccs : CaseClassSkipper[Boolean.Shell2[==, T, std.String, R, std.String]]) =
      ccs(tfs => tfs(this.getValue, r.getValue), this.getValue.asInstanceOf[Any] == r.getValue.asInstanceOf[Any])
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
  implicit object String extends TwoFaceAny.Builder[String, std.String, Shell.One.String, Shell.Two.String, Shell.Three.String] with OpContainer.Eq1[String, std.String] {
    def create[T](value : std.String) : String[T] = new _String[T](value)
    def apply[T <: std.String with Singleton](implicit value : ValueOf[T], di : DummyImplicit, di2 : DummyImplicit) : String[T] = create[T](valueOf[T])
    implicit def apply[T <: std.String with Singleton](value : T) : String[T] = create[T](value)
    implicit def apply[T <: std.String](value : T)(implicit di : DummyImplicit) : String[T] = create[T](value)
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : String[T] = create[T](id.valueWide.asInstanceOf[std.String])
    implicit def tf2NumSingleton[T <: std.String with Singleton](tf : String[T])(implicit value : ValueOf[T]) : T = valueOf[T]
    implicit def tf2NumOp[T](tf : String[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.OutString = id.value.asInstanceOf[id.OutString]
    implicit def tf2NumWide[T](tf : String[T])(implicit di : DummyImplicit, di2 : DummyImplicit) : std.String = tf.getValue
  }

  trait Boolean[T] extends Any with TwoFaceAny[std.Boolean, T] {
    def == (r : std.Boolean)(implicit tfs : Boolean.Shell2[==, T, std.Boolean, GetArg0, std.Boolean]) = tfs(this.getValue, r)
    def != (r : std.Boolean)(implicit tfs : Boolean.Shell2[!=, T, std.Boolean, GetArg0, std.Boolean]) = tfs(this.getValue, r)

    def == [R](r : Boolean[R])(implicit ccs : CaseClassSkipper[Boolean.Shell2[==, T, std.Boolean, R, std.Boolean]]) =
      ccs(tfs => tfs(this.getValue, r.getValue), this.getValue.asInstanceOf[Any] == r.getValue.asInstanceOf[Any])
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
  implicit object Boolean extends TwoFaceAny.Builder[Boolean, std.Boolean, Shell.One.Boolean, Shell.Two.Boolean, Shell.Three.Boolean] with OpContainer.Eq1[Boolean, std.Boolean] {
    def create[T](value : std.Boolean) : Boolean[T] = new _Boolean[T](value)
    def apply[T <: std.Boolean with Singleton](implicit value : ValueOf[T], di : DummyImplicit, di2 : DummyImplicit) : Boolean[T] = create[T](valueOf[T])
    implicit def apply[T <: std.Boolean with Singleton](value : T) : Boolean[T] = create[T](value)
    implicit def apply[T <: std.Boolean](value : T)(implicit di : DummyImplicit) : Boolean[T] = create[T](value)
    implicit def ev[T](implicit id : AcceptNonLiteral[Id[T]]) : Boolean[T] = create[T](id.valueWide.asInstanceOf[std.Boolean])
    implicit def tf2NumSingleton[T <: std.Boolean with Singleton](tf : Boolean[T])(implicit value : ValueOf[T]) : T = valueOf[T]
    implicit def tf2NumOp[T](tf : Boolean[T])(implicit id : AcceptNonLiteral[Id[T]]) : id.OutBoolean = id.value.asInstanceOf[id.OutBoolean]
    implicit def tf2NumWide[T](tf : Boolean[T])(implicit di : DummyImplicit, di2 : DummyImplicit) : std.Boolean = tf.getValue

    //////////////////////////////////////////////////////////////////
    type RequireShell[Cond, Msg, Sym] =
      Shell.Two.Boolean[RequireMsgSym[Cond, Msg, Sym],
        RequireMsgSym[Arg[W.`1`.T, Cond, std.Boolean], Arg[W.`2`.T, Msg, std.String], Sym],
        Cond, std.Boolean, Msg, std.String]
    //////////////////////////////////////////////////////////////////
  }

}
