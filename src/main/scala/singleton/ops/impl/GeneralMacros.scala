package singleton.ops.impl
import macrocompat.bundle
import scala.reflect.macros.whitebox

@bundle
trait GeneralMacros {
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

  ///////////////////////////////////////////////////////////////////////////////////////////
  // One operand (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOp1Gen[F, B, T1, S1](
      implicit ev0: c.WeakTypeTag[F],
      evb: c.WeakTypeTag[B],
      evt1: c.WeakTypeTag[T1],
      evs1: c.WeakTypeTag[S1]): MaterializeOp1AuxGen =
    new MaterializeOp1AuxGen(
      symbolOf[F],
      weakTypeOf[B],
      weakTypeOf[T1],
      weakTypeOf[S1]
    )

  final class MaterializeOp1AuxGen(opSym: TypeSymbol,
                                   bTpe: Type,
                                   t1Tpe: Type,
                                   s1Tpe: Type) {
    def usingFunction[T1, R](f: T1 => R): Tree =
      mkOp1Tree(computeOutValue(f))

    private def computeOutValue[T1, R](f: T1 => R): R = {
      val aValue = extractSingletonValue[T1](s1Tpe)
      f(aValue)
    }

    private def mkOp1Tree[T](outValue: T): Tree =
      mkOpTree(tq"$opSym[$bTpe, $t1Tpe, $s1Tpe]", outValue)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Two operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOp2Gen[F, N, T1, S1, T2, S2](
      implicit ev0: c.WeakTypeTag[F],
      evn: c.WeakTypeTag[N],
      evt1: c.WeakTypeTag[T1],
      evs1: c.WeakTypeTag[S1],
      evt2: c.WeakTypeTag[T2],
      evs2: c.WeakTypeTag[S2]): MaterializeOp2AuxGen =
    new MaterializeOp2AuxGen(
      symbolOf[F],
      weakTypeOf[N],
      weakTypeOf[T1],
      weakTypeOf[S1],
      weakTypeOf[T2],
      weakTypeOf[S2]
    )

  final class MaterializeOp2AuxGen(opSym: TypeSymbol,
                                   nTpe: Type,
                                   t1Tpe: Type,
                                   s1Tpe: Type,
                                   t2Tpe: Type,
                                   s2Tpe: Type) {
    def usingFuncName[N, T1, T2] : Tree = {
      val funcName = extractSingletonValue[N](nTpe).asInstanceOf[String]
      val aValue = extractSingletonValue[T1](s1Tpe)
      val bValue = extractSingletonValue[T2](s2Tpe)

      import scala.math._
      val ((outTpe, outTree), baseTpe) = (funcName, aValue, bValue) match {
        case ("Plus", a : Int,   b : Int)  => (constantTypeAndValueOf[Int](a + b), tq"Int")
        case ("Plus", a : Long,  b : Int)  => (constantTypeAndValueOf[Long](a + b), tq"Long")
        case ("Plus", a : Int,   b : Long) => (constantTypeAndValueOf[Long](a + b), tq"Long")
        case ("Plus", a : Long,  b : Long) => (constantTypeAndValueOf[Long](a + b), tq"Long")
        case _ => abort(s"Unsupported $funcName[$aValue, $bValue]",true)
      }
      val appliedTpe = tq"$opSym[$nTpe, $t1Tpe, $s1Tpe, $t2Tpe, $s2Tpe]"
      q"""
        new $appliedTpe {
          type BaseType = $baseTpe
          type Out = $outTpe
          val value: $outTpe = $outTree
        }
      """
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

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
