package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait I[A] extends Op {
  type Upper = Int
}

object I {
  implicit def materialize[A <: Int](a: A) : I[A] = new I[A] {
    type Out = A
    val value: Out = a
  }
}

//abstract class Mushi[A <: Op](val a : A) extends Op {
//  type Upper = a.Upper
//}
//
//object Mushi {
//  implicit def materialize[A <: Op](a: A) : Mushi[A] = new Mushi[A](a) {
//    type Out = a.Out
//    val value: Out = a.value
//  }
//}
import shapeless._

trait Id[T, A <: T with Singleton] { type Out <: T with Singleton }

object Id {
  def apply[A <: Int with Singleton](implicit id: Id[Int,A]): Aux[Int, A, id.Out] = id
  def apply[A <: Double with Singleton](implicit id: Id[Double, A], di : DummyImplicit): Aux[Double, A, id.Out] = id

  type Aux[T, A <: T with Singleton, B <: T with Singleton] = Id[T,A] { type Out = B }

  implicit def id[T, B <: T with Singleton]: Aux[T, B, B] = new Id[T, B] { type Out = B }
}

object Test {
  val a = Id[1.0]
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
