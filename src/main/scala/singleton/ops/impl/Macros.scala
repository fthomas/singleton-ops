package singleton.ops.impl

import macrocompat.bundle
import scala.reflect.macros.whitebox
@bundle
trait Macros {
  val c: whitebox.Context

  import c.universe._

  ////////////////////////////////////////////////////////////////////
  // Code thanks to Paul Phillips
  // https://github.com/paulp/psply/blob/master/src/main/scala/PsplyMacros.scala
  ////////////////////////////////////////////////////////////////////
  import scala.reflect.internal.SymbolTable
  val g = c.universe.asInstanceOf[SymbolTable]

  implicit def fixSymbolOps(sym: Symbol): g.Symbol = sym.asInstanceOf[g.Symbol]

  /** Typecheck singleton types so as to obtain indirectly
    *  available known-at-compile-time values.
    */
  object Const {
    def unapply(tp: Type): Option[Constant] = tp match {
      case tp @ ExistentialType(_, _) => unapply(tp.underlying)
      case TypeBounds(lo, hi) => unapply(hi)
      case RefinedType(parents, _) =>
        parents.iterator map unapply collectFirst { case Some(x) => x }
      case NullaryMethodType(tpe) => unapply(tpe)
      case TypeRef(_, sym, _) if sym.isAliasType => unapply(tp.dealias)
      case TypeRef(pre, sym, Nil) =>
        unapply(sym.info asSeenFrom (pre, sym.owner))
      case SingleType(pre, sym) =>
        unapply(sym.info asSeenFrom (pre, sym.owner))
      case ConstantType(c) => Some(c)
      case _ => None
    }
  }
  ////////////////////////////////////////////////////////////////////

  def abort(msg: String, usePrint: Boolean = false): Nothing = {
    if (usePrint)
      print(
        "Macro error at position " + c.enclosingPosition + "\nMacro error msg: " + msg)
    c.abort(c.enclosingPosition, msg)
  }

  def constantTypeOf[T](t: T): Type =
    c.internal.constantType(Constant(t))

  def constantTypeAndValueOf[T](t: T): (Type, Tree) =
    (constantTypeOf(t), Literal(Constant(t)))

  def extractSingletonValue[T](tpe: Type): T = {
    def extractionFailed(tpe: Type) = {
      val msg = s"Cannot extract value from $tpe\n" + "showRaw==> " + showRaw(
          tpe)
      abort(msg, true)
    }

    val value = tpe match {
      case Const(Constant(t)) => t
      case _ => extractionFailed(tpe)
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
    def usingFunction[T, R](f: T => R): Tree =
      mkOp1Tree(computeOutValue(f))

    private def computeOutValue[T, R](f: T => R): R =
      f(extractSingletonValue[T](aTpe))

    private def mkOp1Tree[T](outValue: T): Tree =
      mkOpTree(tq"$opSym[$aTpe]", outValue)
  }

  def materializeOp2[F[_, _], A, B](
      implicit ev1: c.WeakTypeTag[F[_, _]],
      ev2: c.WeakTypeTag[A],
      ev3: c.WeakTypeTag[B]
  ): MaterializeOp2Aux =
    new MaterializeOp2Aux(symbolOf[F[_, _]], weakTypeOf[A], weakTypeOf[B])

  final class MaterializeOp2Aux(opSym: TypeSymbol, aTpe: Type, bTpe: Type) {
    def usingFunction[T1, T2, R](f: (T1, T2) => R): Tree =
      mkOp2Tree(computeOutValue(f))

    def usingPredicate[T](f: (T, T) => Boolean): Tree = {
      val outValue = computeOutValue(f)
      if (outValue) {
        mkOp2Tree(outValue)
      } else {
        abort(s"Cannot prove ${opSym.name}[${show(aTpe)}, ${show(bTpe)}]")
      }
    }

    private def computeOutValue[T1, T2, R](f: (T1, T2) => R): R = {
      val aValue = extractSingletonValue[T1](aTpe)
      val bValue = extractSingletonValue[T2](bTpe)
      f(aValue, bValue)
    }

    private def mkOp2Tree[T](outValue: T): Tree =
      mkOpTree(tq"$opSym[$aTpe, $bTpe]", outValue)
  }

  def mkOpTree[T](appliedTpe: Tree, outValue: T): Tree = {
    val (outTpe, outTree) = constantTypeAndValueOf(outValue)
    q"""
      new $appliedTpe {
        type Out = $outTpe
        val value: $outTpe = $outTree
      }
    """
  }
}
