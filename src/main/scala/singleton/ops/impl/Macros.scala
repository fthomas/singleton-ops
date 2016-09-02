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
      case otherTpe => {
        println("Failed extraction for " + tpe + " , see folowing tpe:")
        println(showRaw(tpe))
        extractionFailed(tpe)
      }
    }

    value.asInstanceOf[T]
  }

  def evalTyped[T](expr: c.Expr[T]): T =
    c.eval(c.Expr[T](c.untypecheck(expr.tree)))

  def materializeOp2Gen[F[_, _, _], T, A, B](
      implicit ev1: c.WeakTypeTag[F[_, _, _]],
      ev2: c.WeakTypeTag[T],
      ev3: c.WeakTypeTag[A],
      ev4: c.WeakTypeTag[B]
  ): MaterializeOp2AuxGen =
    new MaterializeOp2AuxGen(symbolOf[F[_, _, _]],
                             weakTypeOf[T],
                             weakTypeOf[A],
                             weakTypeOf[B])


  final class MaterializeOp2AuxGen(opSym: TypeSymbol,
                                   tTpe: Type,
                                   aTpe: Type,
                                   bTpe: Type) {
    def usingFunction[T1, T2, R](f: (T1, T2) => R): Tree =
      mkOp2Tree(computeOutValue(f))

    def usingPredicate[T](f: (T, T) => Boolean): Tree = {
      val outValue = computeOutValue(f)
      if (outValue) {
        mkOp2Tree(outValue)
      } else {
        abort(
          s"Cannot prove ${opSym.name}[${show(tTpe)}, ${show(aTpe)}, ${show(bTpe)}]")
      }
    }

    private def computeOutValue[T1, T2, R](f: (T1, T2) => R): R = {
      val aValue = extractSingletonValue[T1](aTpe)
      val bValue = extractSingletonValue[T2](bTpe)
      f(aValue, bValue)
    }

    private def mkOp2Tree[T](outValue: T): Tree =
      mkOpTree(tq"$opSym[$tTpe, $aTpe, $bTpe]", outValue)
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
