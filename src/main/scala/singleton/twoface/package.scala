//package singleton
//
//import singleton.ops._
//import twoface.impl._
//
//package object twoface {
//  import TwoFaceAny._
//
//  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Char[T], elseArg : Char[E])
//  (implicit tfs : Char.Shell3[ITE, I, scala.Boolean, T, scala.Char, E, scala.Char]) = tfs(ifArg, thenArg, elseArg)
//  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Int[T], elseArg : Int[E])
//  (implicit tfs : Int.Shell3[ITE, I, scala.Boolean, T, scala.Int, E, scala.Int]) = tfs(ifArg, thenArg, elseArg)
//  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Long[T], elseArg : Long[E])
//  (implicit tfs : Long.Shell3[ITE, I, scala.Boolean, T, scala.Long, E, scala.Long]) = tfs(ifArg, thenArg, elseArg)
//  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Float[T], elseArg : Float[E])
//  (implicit tfs : Float.Shell3[ITE, I, scala.Boolean, T, scala.Float, E, scala.Float]) = tfs(ifArg, thenArg, elseArg)
//  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Double[T], elseArg : Double[E])
//  (implicit tfs : Double.Shell3[ITE, I, scala.Boolean, T, scala.Double, E, scala.Double]) = tfs(ifArg, thenArg, elseArg)
//  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : String[T], elseArg : String[E])
//  (implicit tfs : String.Shell3[ITE, I, scala.Boolean, T, java.lang.String, E, java.lang.String]) = tfs(ifArg, thenArg, elseArg)
//  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Boolean[T], elseArg : Boolean[E])
//  (implicit tfs : Boolean.Shell3[ITE, I, scala.Boolean, T, scala.Boolean, E, scala.Boolean]) = tfs(ifArg, thenArg, elseArg)
//
//  def require[C, M](cond : Boolean[C], msg : String[M])(implicit tfs : Boolean.Shell2[RequireMsg, C, scala.Boolean, M, java.lang.String]) = tfs(cond, msg)
//
//  object math {
//    import singleton.ops.math._
//    val Pi = Double.create[Pi](scala.math.Pi)
//    val E = Double.create[E](scala.math.E)
//
//    def abs[T](t : Int[T])(implicit tfs : Int.Shell1[Abs, T, scala.Int]) = tfs(t)
//    def abs[T](t : Long[T])(implicit tfs : Long.Shell1[Abs, T, scala.Long]) = tfs(t)
//    def abs[T](t : Float[T])(implicit tfs : Float.Shell1[Abs, T, scala.Float]) = tfs(t)
//    def abs[T](t : Double[T])(implicit tfs : Double.Shell1[Abs, T, scala.Double]) = tfs(t)
//
//    def min[L, R](l : Int[L], r : Int[R])(implicit tfs : Int.Shell2[Min, L, scala.Int, R, scala.Int]) = tfs(l, r)
//    def min[L, R](l : Long[L], r : Long[R])(implicit tfs : Long.Shell2[Min, L, scala.Long, R, scala.Long]) = tfs(l, r)
//    def min[L, R](l : Float[L], r : Float[R])(implicit tfs : Float.Shell2[Min, L, scala.Float, R, scala.Float]) = tfs(l, r)
//    def min[L, R](l : Double[L], r : Double[R])(implicit tfs : Double.Shell2[Min, L, scala.Double, R, scala.Double]) = tfs(l, r)
//
//    def max[L, R](l : Int[L], r : Int[R])(implicit tfs : Int.Shell2[Max, L, scala.Int, R, scala.Int]) = tfs(l, r)
//    def max[L, R](l : Long[L], r : Long[R])(implicit tfs : Long.Shell2[Max, L, scala.Long, R, scala.Long]) = tfs(l, r)
//    def max[L, R](l : Float[L], r : Float[R])(implicit tfs : Float.Shell2[Max, L, scala.Float, R, scala.Float]) = tfs(l, r)
//    def max[L, R](l : Double[L], r : Double[R])(implicit tfs : Double.Shell2[Max, L, scala.Double, R, scala.Double]) = tfs(l, r)
//
//    def pow[L, R](l : Float[L], r : Float[R])(implicit tfs : Double.Shell2[Pow, L, scala.Float, R, scala.Float]) = tfs(l, r)
//    def pow[L, R](l : Float[L], r : Double[R])(implicit tfs : Double.Shell2[Pow, L, scala.Float, R, scala.Double]) = tfs(l, r)
//    def pow[L, R](l : Double[L], r : Float[R])(implicit tfs : Double.Shell2[Pow, L, scala.Double, R, scala.Float]) = tfs(l, r)
//    def pow[L, R](l : Double[L], r : Double[R])(implicit tfs : Double.Shell2[Pow, L, scala.Double, R, scala.Double]) = tfs(l, r)
//
//    def floor[T](t : Float[T])(implicit tfs : Double.Shell1[Floor, T, scala.Float]) = tfs(t)
//    def floor[T](t : Double[T])(implicit tfs : Double.Shell1[Floor, T, scala.Double]) = tfs(t)
//
//    def ceil[T](t : Float[T])(implicit tfs : Double.Shell1[Ceil, T, scala.Float]) = tfs(t)
//    def ceil[T](t : Double[T])(implicit tfs : Double.Shell1[Ceil, T, scala.Double]) = tfs(t)
//
//    def round[T](t : Float[T])(implicit tfs : Int.Shell1[Round, T, scala.Float]) = tfs(t)
//    def round[T](t : Double[T])(implicit tfs : Long.Shell1[Round, T, scala.Double]) = tfs(t)
//
//    def sin[T](t : Float[T])(implicit tfs : Double.Shell1[Sin, T, scala.Float]) = tfs(t)
//    def sin[T](t : Double[T])(implicit tfs : Double.Shell1[Sin, T, scala.Double]) = tfs(t)
//
//    def cos[T](t : Float[T])(implicit tfs : Double.Shell1[Cos, T, scala.Float]) = tfs(t)
//    def cos[T](t : Double[T])(implicit tfs : Double.Shell1[Cos, T, scala.Double]) = tfs(t)
//
//    def tan[T](t : Float[T])(implicit tfs : Double.Shell1[Tan, T, scala.Float]) = tfs(t)
//    def tan[T](t : Double[T])(implicit tfs : Double.Shell1[Tan, T, scala.Double]) = tfs(t)
//
//    def sqrt[T](t : Float[T])(implicit tfs : Double.Shell1[Sqrt, T, scala.Float]) = tfs(t)
//    def sqrt[T](t : Double[T])(implicit tfs : Double.Shell1[Sqrt, T, scala.Double]) = tfs(t)
//
//    def log[T](t : Float[T])(implicit tfs : Double.Shell1[Log, T, scala.Float]) = tfs(t)
//    def log[T](t : Double[T])(implicit tfs : Double.Shell1[Log, T, scala.Double]) = tfs(t)
//
//    def log10[T](t : Float[T])(implicit tfs : Double.Shell1[Log10, T, scala.Float]) = tfs(t)
//    def log10[T](t : Double[T])(implicit tfs : Double.Shell1[Log10, T, scala.Double]) = tfs(t)
//  }
//}
