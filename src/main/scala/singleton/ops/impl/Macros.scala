package singleton.ops.impl

import macrocompat.bundle
import scala.reflect.macros.whitebox

@bundle
trait Macros {
  val c: whitebox.Context

  import c.universe._

  def abort(msg: String): Nothing =
    c.abort(c.enclosingPosition, msg)

  def constantTypeOf[T](t: T): Type =
    c.internal.constantType(Constant(t))

  def constantTypeAndValueOf[T](t: T): (Type, Tree) =
    (constantTypeOf(t), Literal(Constant(t)))

  def extractSingletonValue[T](tpe: Type): T = {
    def extractionFailed(tpe: Type) =
      abort(s"Cannot extract value from ${tpe.typeSymbol.fullName}")

    val value = tpe match {
      case ConstantType(Constant(t)) => t
      case TypeRef(_, sym, _) =>
        sym.info match {
          case ConstantType(Constant(t)) => t
          case otherTpe => extractionFailed(otherTpe)
        }
      case otherTpe => extractionFailed(otherTpe)
    }

    value.asInstanceOf[T]
  }

  def evalTyped[T](expr: c.Expr[T]): T =
    c.eval(c.Expr[T](c.untypecheck(expr.tree)))

  def materializeOp1[F[_], A](
      implicit ev1: c.WeakTypeTag[F[_]],
      ev2: c.WeakTypeTag[A]
  ): MaterializeOp1Aux =
    new MaterializeOp1Aux(symbolOf[F[_]], weakTypeOf[A])

  final class MaterializeOp1Aux(opSym: TypeSymbol, aTpe: Type) {
    def usingFunction[T](f: T => T): Tree =
      mkOp1Tree(computeOutValue(f))

    private def computeOutValue[T, R](f: T => R): R =
      f(extractSingletonValue[T](aTpe))

    private def mkOp1Tree[T](outValue: T): Tree = {
      val (outTpe, outTree) = constantTypeAndValueOf(outValue)
      q"""
        new $opSym[$aTpe] {
          type Out = $outTpe
          val value: $outTpe = $outTree
        }
      """
    }
  }

  def materializeOp2[F[_, _], A, B](
      implicit ev1: c.WeakTypeTag[F[_, _]],
      ev2: c.WeakTypeTag[A],
      ev3: c.WeakTypeTag[B]
  ): MaterializeOp2Aux =
    new MaterializeOp2Aux(symbolOf[F[_, _]], weakTypeOf[A], weakTypeOf[B])

  final class MaterializeOp2Aux(opSym: TypeSymbol, aTpe: Type, bTpe: Type) {
    def usingFunction[T](f: (T, T) => T): Tree =
      mkOp2Tree(computeOutValue(f))

    def usingPredicate[T](f: (T, T) => Boolean): Tree = {
      val outValue = computeOutValue(f)
      if (outValue) {
        mkOp2Tree(outValue)
      } else {
        abort(s"Cannot prove ${opSym.name}[${show(aTpe)}, ${show(bTpe)}]")
      }
    }

    private def computeOutValue[T, R](f: (T, T) => R): R = {
      val aValue = extractSingletonValue[T](aTpe)
      val bValue = extractSingletonValue[T](bTpe)
      f(aValue, bValue)
    }

    private def mkOp2Tree[T](outValue: T): Tree = {
      val (outTpe, outTree) = constantTypeAndValueOf(outValue)
      q"""
        new $opSym[$aTpe, $bTpe] {
          type Out = $outTpe
          val value: $outTpe = $outTree
        }
      """
    }
  }
}
