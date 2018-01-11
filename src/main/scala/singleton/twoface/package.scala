package singleton

import singleton.ops._
import singleton.ops.impl.std

package object twoface {
  import TwoFace._

  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Char[T], elseArg : Char[E])
  (implicit tfs : Char.Shell3[ITE, I, std.Boolean, T, std.Char, E, std.Char]) = tfs(ifArg.getValue, thenArg.getValue, elseArg.getValue)
  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Int[T], elseArg : Int[E])
  (implicit tfs : Int.Shell3[ITE, I, std.Boolean, T, std.Int, E, std.Int]) = tfs(ifArg.getValue, thenArg.getValue, elseArg.getValue)
  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Long[T], elseArg : Long[E])
  (implicit tfs : Long.Shell3[ITE, I, std.Boolean, T, std.Long, E, std.Long]) = tfs(ifArg.getValue, thenArg.getValue, elseArg.getValue)
  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Float[T], elseArg : Float[E])
  (implicit tfs : Float.Shell3[ITE, I, std.Boolean, T, std.Float, E, std.Float]) = tfs(ifArg.getValue, thenArg.getValue, elseArg.getValue)
  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Double[T], elseArg : Double[E])
  (implicit tfs : Double.Shell3[ITE, I, std.Boolean, T, std.Double, E, std.Double]) = tfs(ifArg.getValue, thenArg.getValue, elseArg.getValue)
  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : String[T], elseArg : String[E])
  (implicit tfs : String.Shell3[ITE, I, std.Boolean, T, std.String, E, std.String]) = tfs(ifArg.getValue, thenArg.getValue, elseArg.getValue)
  def ifThenElse[I, T, E](ifArg: Boolean[I], thenArg : Boolean[T], elseArg : Boolean[E])
  (implicit tfs : Boolean.Shell3[ITE, I, std.Boolean, T, std.Boolean, E, std.Boolean]) = tfs(ifArg.getValue, thenArg.getValue, elseArg.getValue)

  def require[C, M](cond : Boolean[C], msg : String[M])(implicit tfs : Boolean.Shell2[RequireMsg, C, std.Boolean, M, std.String]) = tfs(cond.getValue, msg.getValue)

  object math {
    import singleton.ops.math._
    val Pi = Double.create[Pi](scala.math.Pi)
    val E = Double.create[E](scala.math.E)

    def abs[T](t : Int[T])(implicit tfs : Int.Shell1[Abs, T, std.Int]) = tfs(t.getValue)
    def abs[T](t : Long[T])(implicit tfs : Long.Shell1[Abs, T, std.Long]) = tfs(t.getValue)
    def abs[T](t : Float[T])(implicit tfs : Float.Shell1[Abs, T, std.Float]) = tfs(t.getValue)
    def abs[T](t : Double[T])(implicit tfs : Double.Shell1[Abs, T, std.Double]) = tfs(t.getValue)

    def min[L, R](l : Int[L], r : Int[R])(implicit tfs : Int.Shell2[Min, L, std.Int, R, std.Int]) = tfs(l.getValue, r.getValue)
    def min[L, R](l : Long[L], r : Long[R])(implicit tfs : Long.Shell2[Min, L, std.Long, R, std.Long]) = tfs(l.getValue, r.getValue)
    def min[L, R](l : Float[L], r : Float[R])(implicit tfs : Float.Shell2[Min, L, std.Float, R, std.Float]) = tfs(l.getValue, r.getValue)
    def min[L, R](l : Double[L], r : Double[R])(implicit tfs : Double.Shell2[Min, L, std.Double, R, std.Double]) = tfs(l.getValue, r.getValue)

    def max[L, R](l : Int[L], r : Int[R])(implicit tfs : Int.Shell2[Max, L, std.Int, R, std.Int]) = tfs(l.getValue, r.getValue)
    def max[L, R](l : Long[L], r : Long[R])(implicit tfs : Long.Shell2[Max, L, std.Long, R, std.Long]) = tfs(l.getValue, r.getValue)
    def max[L, R](l : Float[L], r : Float[R])(implicit tfs : Float.Shell2[Max, L, std.Float, R, std.Float]) = tfs(l.getValue, r.getValue)
    def max[L, R](l : Double[L], r : Double[R])(implicit tfs : Double.Shell2[Max, L, std.Double, R, std.Double]) = tfs(l.getValue, r.getValue)

    def pow[L, R](l : Float[L], r : Float[R])(implicit tfs : Double.ShellD[Pow, L, std.Float, R, std.Float]) = tfs(l.getValue, r.getValue)
    def pow[L, R](l : Float[L], r : Double[R])(implicit tfs : Double.ShellD[Pow, L, std.Float, R, std.Double]) = tfs(l.getValue, r.getValue)
    def pow[L, R](l : Double[L], r : Float[R])(implicit tfs : Double.ShellD[Pow, L, std.Double, R, std.Float]) = tfs(l.getValue, r.getValue)
    def pow[L, R](l : Double[L], r : Double[R])(implicit tfs : Double.Shell2[Pow, L, std.Double, R, std.Double]) = tfs(l.getValue, r.getValue)

    def floor[T](t : Float[T])(implicit tfs : Double.Shell1D[Floor, T, std.Float]) = tfs(t.getValue)
    def floor[T](t : Double[T])(implicit tfs : Double.Shell1[Floor, T, std.Double]) = tfs(t.getValue)

    def ceil[T](t : Float[T])(implicit tfs : Double.Shell1D[Ceil, T, std.Float]) = tfs(t.getValue)
    def ceil[T](t : Double[T])(implicit tfs : Double.Shell1[Ceil, T, std.Double]) = tfs(t.getValue)

    def round[T](t : Float[T])(implicit tfs : Int.Shell1[Round, T, std.Float]) = tfs(t.getValue)
    def round[T](t : Double[T])(implicit tfs : Long.Shell1[Round, T, std.Double]) = tfs(t.getValue)

    def sin[T](t : Float[T])(implicit tfs : Double.Shell1D[Sin, T, std.Float]) = tfs(t.getValue)
    def sin[T](t : Double[T])(implicit tfs : Double.Shell1[Sin, T, std.Double]) = tfs(t.getValue)

    def cos[T](t : Float[T])(implicit tfs : Double.Shell1D[Cos, T, std.Float]) = tfs(t.getValue)
    def cos[T](t : Double[T])(implicit tfs : Double.Shell1[Cos, T, std.Double]) = tfs(t.getValue)

    def tan[T](t : Float[T])(implicit tfs : Double.Shell1D[Tan, T, std.Float]) = tfs(t.getValue)
    def tan[T](t : Double[T])(implicit tfs : Double.Shell1[Tan, T, std.Double]) = tfs(t.getValue)

    def sqrt[T](t : Float[T])(implicit tfs : Double.Shell1D[Sqrt, T, std.Float]) = tfs(t.getValue)
    def sqrt[T](t : Double[T])(implicit tfs : Double.Shell1[Sqrt, T, std.Double]) = tfs(t.getValue)

    def log[T](t : Float[T])(implicit tfs : Double.Shell1D[Log, T, std.Float]) = tfs(t.getValue)
    def log[T](t : Double[T])(implicit tfs : Double.Shell1[Log, T, std.Double]) = tfs(t.getValue)

    def log10[T](t : Float[T])(implicit tfs : Double.Shell1D[Log10, T, std.Float]) = tfs(t.getValue)
    def log10[T](t : Double[T])(implicit tfs : Double.Shell1[Log10, T, std.Double]) = tfs(t.getValue)
  }
}
