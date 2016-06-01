package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox

@bundle
trait MacroUtils {
  val c: whitebox.Context

  import c.universe._

  def constantTypeOf[A](a: A): Type =
    c.internal.constantType(Constant(a))

  def extractSingletonValue[A](tpe: Type): A = {
    def abort(tpe: Type) =
      c.abort(c.enclosingPosition,
              s"Cannot extract value from ${tpe.typeSymbol.fullName}")

    val value = tpe match {
      case ConstantType(Constant(a)) => a
      case TypeRef(_, sym, _) =>
        sym.info match {
          case ConstantType(Constant(a)) => a
          case otherTpe => abort(otherTpe)
        }
      case otherTpe => abort(otherTpe)
    }

    value.asInstanceOf[A]
  }

  def evalTyped[A](expr: c.Expr[A]): A =
    c.eval(c.Expr[A](c.untypecheck(expr.tree)))

  def materializeBinaryOp[F[_, _], A, B](
      implicit ev1: c.WeakTypeTag[F[_, _]],
      ev2: c.WeakTypeTag[A],
      ev3: c.WeakTypeTag[B]
  ): MaterializeBinaryOpAux =
    new MaterializeBinaryOpAux(symbolOf[F[_, _]], weakTypeOf[A], weakTypeOf[B])

  final class MaterializeBinaryOpAux(opSym: TypeSymbol, aTpe: Type, bTpe: Type) {
    def apply[T](f: (T, T) => T): Tree = {
      val aValue = extractSingletonValue[T](aTpe)
      val bValue = extractSingletonValue[T](bTpe)
      val abTpe = constantTypeOf(f(aValue, bValue))

      q"new $opSym[$aTpe, $bTpe] { type Out = $abTpe }"
    }
  }
}
