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
    def abort(tpeInfo: String) =
      c.abort(c.enclosingPosition, s"Cannot extract value from $tpeInfo")

    tpe match {
      case ConstantType(Constant(a)) => a.asInstanceOf[A]
      case TypeRef(_, sym, _) =>
        sym.info match {
          case ConstantType(Constant(a)) => a.asInstanceOf[A]
          case _ => abort(sym.info.toString)
        }
      case _ => abort(tpe.toString)
    }
  }
}
