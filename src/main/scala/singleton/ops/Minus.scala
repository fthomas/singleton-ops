package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Minus[T, A, B] extends Op {
  override type Out <: T
}

//object Minus extends Op2CompanionGen[Minus] {
//  implicit def materializeMinus[T, A <: T with Singleton,
//  B <: T with Singleton](
//      implicit nt: Numeric[T]
//  ): Minus[T, A, B] = macro MinusMacro.materialize[T, A, B]
//
//  @bundle
//  final class MinusMacro(val c: whitebox.Context) extends Macros {
//    def materialize[T: c.WeakTypeTag,
//                    A <: T with Singleton: c.WeakTypeTag,
//                    B <: T with Singleton: c.WeakTypeTag](
//        nt: c.Expr[Numeric[T]]
//    ): c.Tree =
//      materializeOp2GenMinus[Minus, T, A, B].usingFunction(evalTyped(nt).minus)
//  }
//}

//trait Minus[T, A <: T with Singleton, B <: T with Singleton]

object Minus extends Op2CompanionGen[Minus] {
  import scala.reflect.macros.whitebox.Context
  import scala.language.experimental.macros

  def helper[T: c.WeakTypeTag,
             A <: T with Singleton: c.WeakTypeTag,
             B <: T with Singleton: c.WeakTypeTag](
      c: Context)(nt: c.Expr[Numeric[T]]): c.Expr[Minus[T, A, B]] = {
    import c.universe._
    val weakT = weakTypeOf[T]
    val weakA = weakTypeOf[A]
    val weakB = weakTypeOf[B]
    val aTree = tq"$weakA"
    val bTree = tq"$weakB"
    val tTree = tq"$weakT"
    val structTree = tq"_root_.singleton.ops.Minus"
    val att = AppliedTypeTree(structTree, List(tTree, aTree, bTree))
    val className = c.freshName()
    val classType = TypeName(className)
    val classTerm = TermName(className)
    val genTree = q"""
        new $att {
          type Out = 1
          val value : Out = 1
        }
      """
//    val genTree = q"""
//        case class $classType() extends ..$list {
//          def newEmptyDFVar = copy().asInstanceOf[TVar]
//        }
//        $classTerm()
//      """
    c.Expr(genTree)
  }
  implicit def materializeMinus[T, A <: T with Singleton, B <: T with Singleton](implicit nt: Numeric[T]): Minus[T, A, B] =
    macro helper[T, A, B]
}
