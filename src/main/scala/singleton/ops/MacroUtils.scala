package singleton.ops

import macrocompat.bundle
import scala.reflect.macros.whitebox

@bundle
class MacroUtils(val c: whitebox.Context) {
  import c.universe._

  def materializeHelper[C](f: (C, C) => C)(aTpe: Type, bTpe: Type)(
      mkRefinedTree: (Type, Type, Type) => Tree): Tree = {
    val aValue = extractSingletonValue[C](aTpe)
    val bValue = extractSingletonValue[C](bTpe)
    val abTpe = constantTypeOf(f(aValue, bValue))
    mkRefinedTree(aTpe, bTpe, abTpe)
  }

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

  def mkBinaryTypeClass(tpeSym: TypeSymbol): (Type, Type, Type) => Tree =
    (a, b, c) => q"new $tpeSym[$a, $b] { type Out = $c }"
}
