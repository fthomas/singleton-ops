package singleton.ops.macros

import macrocompat.bundle
import scala.reflect.macros.whitebox

@bundle
trait MacroUtils {
  val c: whitebox.Context

  import c.universe._

  def constantTypeOf[T](t: T): Type =
    c.internal.constantType(Constant(t))

  def extractSingletonValue[T](tpe: Type): T = {
    def abort(tpe: Type) =
      c.abort(c.enclosingPosition,
              s"Cannot extract value from ${tpe.typeSymbol.fullName}")

    val value = tpe match {
      case ConstantType(Constant(t)) => t
      case TypeRef(_, sym, _) =>
        sym.info match {
          case ConstantType(Constant(t)) => t
          case otherTpe => abort(otherTpe)
        }
      case otherTpe => abort(otherTpe)
    }

    value.asInstanceOf[T]
  }

  def evalTyped[T](expr: c.Expr[T]): T =
    c.eval(c.Expr[T](c.untypecheck(expr.tree)))

  def materializeBinaryOp[Op[_, _], A, B](
      implicit ev1: c.WeakTypeTag[Op[_, _]],
      ev2: c.WeakTypeTag[A],
      ev3: c.WeakTypeTag[B]
  ): MaterializeBinaryOpAux =
    new MaterializeBinaryOpAux(
        symbolOf[Op[_, _]], weakTypeOf[A], weakTypeOf[B])

  final class MaterializeBinaryOpAux(opSym: TypeSymbol, aTpe: Type, bTpe: Type) {
    def apply[T](f: (T, T) => T): Tree = {
      val aValue = extractSingletonValue[T](aTpe)
      val bValue = extractSingletonValue[T](bTpe)
      val outTpe = constantTypeOf(f(aValue, bValue))

      q"new $opSym[$aTpe, $bTpe] { type Out = $outTpe }"
    }
  }
}
