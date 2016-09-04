package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._


trait Opy[Upper] extends Serializable {
  type Out <: Upper with Singleton
  val value : Out {}
}


trait Sum[T, A <: T with Singleton, B <: T with Singleton] extends Opy[T]

object Sum {
  def apply[T, A <: T with Singleton, B <: T with Singleton]
  (implicit sum: Sum[T, A, B]): Aux[T, A, B, sum.Out] = sum

  def apply[A <: Int with Singleton, B <: Int with Singleton]
  (implicit sum: Sum[Int, A, B], di1 : DummyImplicit) : Aux[Int, A, B, sum.Out] = sum

  def apply[A <: Double with Singleton, B <: Double with Singleton]
  (implicit sum: Sum[Double, A, B], di1 : DummyImplicit, di2 : DummyImplicit) : Aux[Double, A, B, sum.Out] = sum

  type Aux[T, A <: T with Singleton, B <: T with Singleton, C <: T with Singleton] = Sum[T, A, B] { type Out = C }

  implicit def macroCall[T, A <: T with Singleton, B <: T with Singleton]
  (implicit nt: Numeric[T]):
  Sum[T, A, B] = macro Macro.impl[Sum[_,_,_],T, A, B]

  @bundle
  final class Macro(val c: whitebox.Context) extends Macros {
    def impl[
    F <: Serializable : c.WeakTypeTag,
    T : c.WeakTypeTag,
    A <: T with Singleton : c.WeakTypeTag,
    B <: T with Singleton : c.WeakTypeTag]
    (nt: c.Expr[Numeric[T]]):
    c.Tree = materializeOp3[F, T, A, B].usingFunction(evalTyped(nt).plus)
  }
}



//abstract class Plus[A <: Op, B <: Op](val a: A, b: B) extends Op {
//  type Upper = a.Upper
//}
//
//object Plus extends Op2CompanionPlus[Plus] {
//  implicit def materialize[A <: Op, B <: Op](a: A, b: B) : Plus[A, B] = {
//
//    new Plus[A,B](a, b) {
//      type Out = a.Out
//      val value: Out = a.value
//    }
//
//  }
//  //    //val materializePlus[a.Upper, a.Out, b.Out]
//  //
//  //  def materializePlus[T, A <: T, B <: T](
//  //      implicit nt: Numeric[T]
//  //  ): Plus[A, B] = macro PlusMacro.materialize[T, A, B]
//  //
//  //  @bundle
//  //  final class PlusMacro(val c: whitebox.Context) extends Macros {
//  //    def materialize[T, A: c.WeakTypeTag, B: c.WeakTypeTag](
//  //        nt: c.Expr[Numeric[T]]
//  //    ): c.Tree =
//  //      materializeOp2Plus[Plus, A, B].usingFunction(evalTyped(nt).plus)
//  //  }
//}
//
