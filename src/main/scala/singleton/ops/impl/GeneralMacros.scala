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

  ///////////////////////////////////////////////////////////////////////////////////////////
  // One operand (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOp1Gen[F, N, T1, S1](
    implicit ev0: c.WeakTypeTag[F],
    evn: c.WeakTypeTag[N],
    evt1: c.WeakTypeTag[T1],
    evs1: c.WeakTypeTag[S1]): MaterializeOp1AuxGen =
  new MaterializeOp1AuxGen(
    symbolOf[F],
    weakTypeOf[N],
    weakTypeOf[T1],
    weakTypeOf[S1])

  final class MaterializeOp1AuxGen(opSym: TypeSymbol,
                                   nTpe: Type,
                                   t1Tpe: Type,
                                   s1Tpe: Type) {
    def usingFuncName[N, T1, T2] : Tree = {
      val funcName = extractSingletonValue[N](nTpe).asInstanceOf[String]
      val aValue = extractSingletonValue[T1](s1Tpe)

      import scala.math._
      val ((outTpe, outTree), baseTpe) = (funcName, aValue) match {
        case ("ToInt",    a : Int)      => (constantTypeAndValueOf[Int](a.toInt), tq"Int")
        case ("ToInt",    a : Long)     => (constantTypeAndValueOf[Int](a.toInt), tq"Int")
        case ("ToInt",    a : Double)   => (constantTypeAndValueOf[Int](a.toInt), tq"Int")
        case ("ToLong",   a : Int)      => (constantTypeAndValueOf[Long](a.toLong), tq"Long")
        case ("ToLong",   a : Long)     => (constantTypeAndValueOf[Long](a.toLong), tq"Long")
        case ("ToLong",   a : Double)   => (constantTypeAndValueOf[Long](a.toLong), tq"Long")
        case ("ToDouble", a : Int)      => (constantTypeAndValueOf[Double](a.toDouble), tq"Double")
        case ("ToDouble", a : Long)     => (constantTypeAndValueOf[Double](a.toDouble), tq"Double")
        case ("ToDouble", a : Double)   => (constantTypeAndValueOf[Double](a.toDouble), tq"Double")
        case ("Negate",   a : Int)      => (constantTypeAndValueOf[Int](-a), tq"Int")
        case ("Negate",   a : Long)     => (constantTypeAndValueOf[Long](-a), tq"Long")
        case ("Negate",   a : Double)   => (constantTypeAndValueOf[Double](-a), tq"Double")
        case ("Abs",      a : Int)      => (constantTypeAndValueOf[Int](abs(a)), tq"Int")
        case ("Abs",      a : Long)     => (constantTypeAndValueOf[Long](abs(a)), tq"Long")
        case ("Abs",      a : Double)   => (constantTypeAndValueOf[Double](abs(a)), tq"Double")
        case ("Reverse",  a : String)   => (constantTypeAndValueOf[String](a.reverse), tq"String")
        case ("!",        a : Boolean)  => (constantTypeAndValueOf[Boolean](!a), tq"Boolean")
        case ("Require",  a : Boolean)  =>
          if (!a)
            abort(s"Cannot prove requirement Require[...]", true)
          else
            (constantTypeAndValueOf[Boolean](a), tq"Boolean")
        case _ => abort(s"Unsupported $funcName[$aValue]",true)
      }
      val appliedTpe = tq"$opSym[$nTpe, $t1Tpe, $s1Tpe]"
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
        case ("+",            a : Int,     b : Int)     => (constantTypeAndValueOf[Int](a + b), tq"Int")
        case ("+",            a : Int,     b : Long)    => (constantTypeAndValueOf[Long](a + b), tq"Long")
        case ("+",            a : Int,     b : Double)  => (constantTypeAndValueOf[Double](a + b), tq"Double")
        case ("+",            a : Long,    b : Int)     => (constantTypeAndValueOf[Long](a + b), tq"Long")
        case ("+",            a : Long,    b : Long)    => (constantTypeAndValueOf[Long](a + b), tq"Long")
        case ("+",            a : Long,    b : Double)  => (constantTypeAndValueOf[Double](a + b), tq"Double")
        case ("+",            a : Double,  b : Int)     => (constantTypeAndValueOf[Double](a + b), tq"Double")
        case ("+",            a : Double,  b : Long)    => (constantTypeAndValueOf[Double](a + b), tq"Double")
        case ("+",            a : Double,  b : Double)  => (constantTypeAndValueOf[Double](a + b), tq"Double")
        case ("+",            a : String,  b : String)  => (constantTypeAndValueOf[String](a + b), tq"String")

        case ("-",            a : Int,     b : Int)     => (constantTypeAndValueOf[Int](a - b), tq"Int")
        case ("-",            a : Int,     b : Long)    => (constantTypeAndValueOf[Long](a - b), tq"Long")
        case ("-",            a : Int,     b : Double)  => (constantTypeAndValueOf[Double](a - b), tq"Double")
        case ("-",            a : Long,    b : Int)     => (constantTypeAndValueOf[Long](a - b), tq"Long")
        case ("-",            a : Long,    b : Long)    => (constantTypeAndValueOf[Long](a - b), tq"Long")
        case ("-",            a : Long,    b : Double)  => (constantTypeAndValueOf[Double](a - b), tq"Double")
        case ("-",            a : Double,  b : Int)     => (constantTypeAndValueOf[Double](a - b), tq"Double")
        case ("-",            a : Double,  b : Long)    => (constantTypeAndValueOf[Double](a - b), tq"Double")
        case ("-",            a : Double,  b : Double)  => (constantTypeAndValueOf[Double](a - b), tq"Double")

        case ("*",            a : Int,     b : Int)     => (constantTypeAndValueOf[Int](a * b), tq"Int")
        case ("*",            a : Int,     b : Double)  => (constantTypeAndValueOf[Double](a * b), tq"Double")
        case ("*",            a : Long,    b : Int)     => (constantTypeAndValueOf[Long](a * b), tq"Long")
        case ("*",            a : Long,    b : Long)    => (constantTypeAndValueOf[Long](a * b), tq"Long")
        case ("*",            a : Long,    b : Double)  => (constantTypeAndValueOf[Double](a * b), tq"Double")
        case ("*",            a : Double,  b : Int)     => (constantTypeAndValueOf[Double](a * b), tq"Double")
        case ("*",            a : Double,  b : Long)    => (constantTypeAndValueOf[Double](a * b), tq"Double")
        case ("*",            a : Double,  b : Double)  => (constantTypeAndValueOf[Double](a * b), tq"Double")

        case ("/",            a : Int,     b : Int)     => (constantTypeAndValueOf[Int](a / b), tq"Int")
        case ("/",            a : Int,     b : Long)    => (constantTypeAndValueOf[Long](a / b), tq"Long")
        case ("/",            a : Int,     b : Double)  => (constantTypeAndValueOf[Double](a / b), tq"Double")
        case ("/",            a : Long,    b : Int)     => (constantTypeAndValueOf[Long](a / b), tq"Long")
        case ("/",            a : Long,    b : Long)    => (constantTypeAndValueOf[Long](a / b), tq"Long")
        case ("/",            a : Long,    b : Double)  => (constantTypeAndValueOf[Double](a / b), tq"Double")
        case ("/",            a : Double,  b : Int)     => (constantTypeAndValueOf[Double](a / b), tq"Double")
        case ("/",            a : Double,  b : Long)    => (constantTypeAndValueOf[Double](a / b), tq"Double")
        case ("/",            a : Double,  b : Double)  => (constantTypeAndValueOf[Double](a / b), tq"Double")

        case ("<",            a : Int,     b : Int)     => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Int,     b : Long)    => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Int,     b : Double)  => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Long,    b : Int)     => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Long,    b : Long)    => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Long,    b : Double)  => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Double,  b : Int)     => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Double,  b : Long)    => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")
        case ("<",            a : Double,  b : Double)  => (constantTypeAndValueOf[Boolean](a < b), tq"Boolean")

        case ("==",           a : Int,     b : Int)    => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Int,     b : Long)   => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Int,     b : Double) => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Long,    b : Int)    => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Long,    b : Long)   => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Long,    b : Double) => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Double,  b : Int)    => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Double,  b : Long)   => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")
        case ("==",           a : Double,  b : Double) => (constantTypeAndValueOf[Boolean](a == b), tq"Boolean")

        case ("!=",           a : Int,     b : Int)    => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Int,     b : Long)   => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Int,     b : Double) => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Long,    b : Int)    => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Long,    b : Long)   => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Long,    b : Double) => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Double,  b : Int)    => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Double,  b : Long)   => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")
        case ("!=",           a : Double,  b : Double) => (constantTypeAndValueOf[Boolean](a != b), tq"Boolean")

        case (">",            a : Int,     b : Int)     => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Int,     b : Long)    => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Int,     b : Double)  => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Long,    b : Int)     => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Long,    b : Long)    => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Long,    b : Double)  => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Double,  b : Int)     => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Double,  b : Long)    => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")
        case (">",            a : Double,  b : Double)  => (constantTypeAndValueOf[Boolean](a > b), tq"Boolean")

        case ("<=",           a : Int,     b : Int)     => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Int,     b : Long)    => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Int,     b : Double)  => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Long,    b : Int)     => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Long,    b : Long)    => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Long,    b : Double)  => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Double,  b : Int)     => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Double,  b : Long)    => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")
        case ("<=",           a : Double,  b : Double)  => (constantTypeAndValueOf[Boolean](a <= b), tq"Boolean")

        case (">=",           a : Int,     b : Int)     => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Int,     b : Long)    => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Int,     b : Double)  => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Long,    b : Int)     => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Long,    b : Long)    => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Long,    b : Double)  => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Double,  b : Int)     => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Double,  b : Long)    => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")
        case (">=",           a : Double,  b : Double)  => (constantTypeAndValueOf[Boolean](a >= b), tq"Boolean")

        case ("&&",           a : Boolean, b : Boolean) => (constantTypeAndValueOf[Boolean](a && b), tq"Boolean")
        case ("||",           a : Boolean, b : Boolean) => (constantTypeAndValueOf[Boolean](a || b), tq"Boolean")

        case ("Min",          a : Int,     b : Int)     => (constantTypeAndValueOf[Int](min(a,b)), tq"Int")
        case ("Min",          a : Long,    b : Long)    => (constantTypeAndValueOf[Long](min(a,b)), tq"Long")
        case ("Min",          a : Double,  b : Double)  => (constantTypeAndValueOf[Double](min(a,b)), tq"Double")

        case ("Max",          a : Int,     b : Int)     => (constantTypeAndValueOf[Int](max(a,b)), tq"Int")
        case ("Max",          a : Long,    b : Long)    => (constantTypeAndValueOf[Long](max(a,b)), tq"Long")
        case ("Max",          a : Double,  b : Double)  => (constantTypeAndValueOf[Double](max(a,b)), tq"Double")
          
        case ("Substring",    a : String,  b : Int)     => (constantTypeAndValueOf[String](a.substring(b)), tq"String")

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

}
