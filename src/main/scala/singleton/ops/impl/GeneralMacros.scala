package singleton.ops.impl
import macrocompat.bundle
import shapeless.Nat

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
    def unapply(tp: Type): Option[Constant] = {
//      print(showRaw(tp))
      tp match {
        case tp @ ExistentialType(_, _) => unapply(tp.underlying)
        case TypeBounds(lo, hi) => unapply(hi)
        case RefinedType(parents, _) =>
          parents.iterator map unapply collectFirst { case Some(x) => x }
        case NullaryMethodType(tpe) => unapply(tpe)
        ////////////////////////////////////////////////////////////////////////
        // For Shapeless Nat
        ////////////////////////////////////////////////////////////////////////
        case TypeRef(_, sym, args) if sym == symbolOf[shapeless.Succ[_]] =>
          val next = unapply(args.head)
          next match {
            case Some(Constant(t: Int)) => Some(Constant(t + 1))
            case _ => None
          }
        case TypeRef(_, sym, _) if sym == symbolOf[shapeless._0] =>
          Some(Constant(0))
        ////////////////////////////////////////////////////////////////////////

        case TypeRef(_, sym, args) if sym == symbolOf[Op1Macro[_,_]] =>
          val funcName = unapply(args.head)
          val aValue = unapply(args(1))
          (funcName, aValue) match {
            case (Some(Constant(f : String)), Some(Constant(a))) => op1Calc(f, a)
            case _ => None
          }

        case TypeRef(_, sym, args) if sym == symbolOf[Op2Macro[_,_,_]] =>
          val funcName = unapply(args.head)
          val aValue = unapply(args(1))
          val bValue = unapply(args(2))
          (funcName, aValue, bValue) match {
            case (Some(Constant(f : String)), Some(Constant(a)), Some(Constant(b))) => op2Calc(f, a, b)
            case _ => None
          }

        case TypeRef(_, sym, _) if sym.isAliasType => unapply(tp.dealias)
        case TypeRef(pre, sym, Nil) =>
          unapply(sym.info asSeenFrom (pre, sym.owner))
        case SingleType(pre, sym) =>
          unapply(sym.info asSeenFrom (pre, sym.owner))
        case ConstantType(c) => Some(c)
        case _ =>
//          print("Exhausted search at: " + showRaw(tp))
          None
      }
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

  def constantTypeAndValueOf[T](t: T)(
      implicit ev: c.WeakTypeTag[T]): (Type, Literal, TypeName, Type, Tree) ={
    val outWideType = weakTypeOf[T]
    val outWideLiteral = Literal(Constant(t))
    val outTypeName = t match {
      case tt : Nat => TypeName("OutNat")
      case tt : Int => TypeName("OutInt")
      case tt : Long => TypeName("OutLong")
      case tt : Double => TypeName("OutDouble")
      case tt : String => TypeName("OutString")
      case tt : Boolean => TypeName("OutBoolean")
    }
    (outWideType, outWideLiteral, outTypeName, constantTypeOf(t), Literal(Constant(t)))
  }

  def constantTypeAndValueOfNat(t: Int): (Type, Literal, TypeName, Type, Tree) ={
    val outWideType = typeOf[Int]
    val outWideLiteral = Literal(Constant(t))
    val outTypeName = TypeName("OutNat")
    (outWideType, outWideLiteral, outTypeName, mkNatTpe(t), q"new ${mkNatTpt(t)}")
  }

  def extractSingletonValue[T](tpe: Type): T = {
    def extractionFailed(tpe: Type, usePrint : Boolean) = {
      val msg = s"Cannot extract value from $tpe\n" + "showRaw==> " + showRaw(
          tpe)
      abort(msg, usePrint)
    }

    val value = tpe match {
      case Const(Constant(t)) => t
      case _ => extractionFailed(tpe, true)
    }

    value.asInstanceOf[T]
  }

  def evalTyped[T](expr: c.Expr[T]): T =
    c.eval(c.Expr[T](c.untypecheck(expr.tree)))

  ///////////////////////////////////////////////////////////////////////////////////////////
  // One operand (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOp1Gen[F, N, S1](
      implicit ev0: c.WeakTypeTag[F],
      evn: c.WeakTypeTag[N],
      evs1: c.WeakTypeTag[S1]): MaterializeOp1AuxGen =
    new MaterializeOp1AuxGen(symbolOf[F], weakTypeOf[N], weakTypeOf[S1])

  def op1Calc[T1](funcName : String, aValue : T1) : Option[Constant] = {
    import scala.math._
    val ret = (funcName, aValue) match {
      case ("Id", a: Int) => Some(Constant(a))
      case ("Id", a: Long) => Some(Constant(a))
      case ("Id", a: Double) => Some(Constant(a))
      case ("Id", a: String) => Some(Constant(a))
      case ("Id", a: Boolean) => Some(Constant(a))
      case ("ToNat", a: Int) => Some(Constant(a))
      case ("ToInt", a: Int) => Some(Constant(a.toInt))
      case ("ToInt", a: Long) => Some(Constant(a.toInt))
      case ("ToInt", a: Double) => Some(Constant(a.toInt))
      case ("ToLong", a: Int) => Some(Constant(a.toLong))
      case ("ToLong", a: Long) => Some(Constant(a.toLong))
      case ("ToLong", a: Double) => Some(Constant(a.toLong))
      case ("ToDouble", a: Int) => Some(Constant(a.toDouble))
      case ("ToDouble", a: Long) => Some(Constant(a.toDouble))
      case ("ToDouble", a: Double) => Some(Constant(a.toDouble))
      case ("Negate", a: Int) => Some(Constant(-a))
      case ("Negate", a: Long) => Some(Constant(-a))
      case ("Negate", a: Double) => Some(Constant(-a))
      case ("Abs", a: Int) => Some(Constant(abs(a)))
      case ("Abs", a: Long) => Some(Constant(abs(a)))
      case ("Abs", a: Double) => Some(Constant(abs(a)))
      case ("Reverse", a: String) => Some(Constant(a.reverse))
      case ("!", a: Boolean) => Some(Constant(!a))
      case ("Require", a: Boolean) =>
        if (!a)
          abort(s"Cannot prove requirement Require[...]", false)
        else
          Some(Constant(a))
      case _ => abort(s"Unsupported $funcName[$aValue]", true)
    }
    ret
  }
  final class MaterializeOp1AuxGen(opSym: TypeSymbol, nTpe: Type, s1Tpe: Type) {
    def usingFuncName[N, T1, T2]: Tree = {
      val funcName = extractSingletonValue[N](nTpe).asInstanceOf[String]
      val aValue = extractSingletonValue[T1](s1Tpe)
      val (outWideTpe, outWideLiteral, outTypeName, outTpe, outTree) = (funcName, op1Calc(funcName,aValue)) match {
        case ("ToNat", Some(Constant(t : Int))) => constantTypeAndValueOfNat(t)
        case (_, Some(Constant(t))) =>  constantTypeAndValueOf(t)
        case _ => abort(s"Unsupported $funcName[$aValue]", true)
      }
      val appliedTpe = tq"$opSym[$nTpe, $s1Tpe]"
      val genTree = q"""
        new $appliedTpe {
          type OutWide = $outWideTpe
          type Out = $outTpe
          type $outTypeName = $outTpe
          val value: $outTpe = $outTree
          val valueWide: $outWideTpe = $outWideLiteral
        }
      """
//      print(genTree)
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Two operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOp2Gen[F, N, S1, S2](
      implicit ev0: c.WeakTypeTag[F],
      evn: c.WeakTypeTag[N],
      evs1: c.WeakTypeTag[S1],
      evs2: c.WeakTypeTag[S2]): MaterializeOp2AuxGen =
    new MaterializeOp2AuxGen(
      symbolOf[F],
      weakTypeOf[N],
      weakTypeOf[S1],
      weakTypeOf[S2]
    )

  def op2Calc[T1, T2](funcName : String, aValue : T1, bValue : T2) : Option[Constant] = {
    import scala.math._
    val ret = (funcName, aValue, bValue) match {
      case ("+", a: Int, b: Int) => Some(Constant(a + b))
      case ("+", a: Int, b: Long) => Some(Constant(a + b))
      case ("+", a: Int, b: Double) => Some(Constant(a + b))
      case ("+", a: Long, b: Int) => Some(Constant(a + b))
      case ("+", a: Long, b: Long) => Some(Constant(a + b))
      case ("+", a: Long, b: Double) => Some(Constant(a + b))
      case ("+", a: Double, b: Int) => Some(Constant(a + b))
      case ("+", a: Double, b: Long) => Some(Constant(a + b))
      case ("+", a: Double, b: Double) => Some(Constant(a + b))
      case ("+", a: String, b: String) => Some(Constant(a + b))

      case ("-", a: Int, b: Int) => Some(Constant(a - b))
      case ("-", a: Int, b: Long) => Some(Constant(a - b))
      case ("-", a: Int, b: Double) => Some(Constant(a - b))
      case ("-", a: Long, b: Int) => Some(Constant(a - b))
      case ("-", a: Long, b: Long) => Some(Constant(a - b))
      case ("-", a: Long, b: Double) => Some(Constant(a - b))
      case ("-", a: Double, b: Int) => Some(Constant(a - b))
      case ("-", a: Double, b: Long) => Some(Constant(a - b))
      case ("-", a: Double, b: Double) => Some(Constant(a - b))

      case ("*", a: Int, b: Int) => Some(Constant(a * b))
      case ("*", a: Int, b: Double) => Some(Constant(a * b))
      case ("*", a: Long, b: Int) => Some(Constant(a * b))
      case ("*", a: Long, b: Long) => Some(Constant(a * b))
      case ("*", a: Long, b: Double) => Some(Constant(a * b))
      case ("*", a: Double, b: Int) => Some(Constant(a * b))
      case ("*", a: Double, b: Long) => Some(Constant(a * b))
      case ("*", a: Double, b: Double) => Some(Constant(a * b))

      case ("/", a: Int, b: Int) => Some(Constant(a / b))
      case ("/", a: Int, b: Long) => Some(Constant(a / b))
      case ("/", a: Int, b: Double) => Some(Constant(a / b))
      case ("/", a: Long, b: Int) => Some(Constant(a / b))
      case ("/", a: Long, b: Long) => Some(Constant(a / b))
      case ("/", a: Long, b: Double) => Some(Constant(a / b))
      case ("/", a: Double, b: Int) => Some(Constant(a / b))
      case ("/", a: Double, b: Long) => Some(Constant(a / b))
      case ("/", a: Double, b: Double) => Some(Constant(a / b))

      case ("<", a: Int, b: Int) => Some(Constant(a < b))
      case ("<", a: Int, b: Long) => Some(Constant(a < b))
      case ("<", a: Int, b: Double) => Some(Constant(a < b))
      case ("<", a: Long, b: Int) => Some(Constant(a < b))
      case ("<", a: Long, b: Long) => Some(Constant(a < b))
      case ("<", a: Long, b: Double) => Some(Constant(a < b))
      case ("<", a: Double, b: Int) => Some(Constant(a < b))
      case ("<", a: Double, b: Long) => Some(Constant(a < b))
      case ("<", a: Double, b: Double) => Some(Constant(a < b))

      case ("==", a: Int, b: Int) => Some(Constant(a == b))
      case ("==", a: Int, b: Long) => Some(Constant(a == b))
      case ("==", a: Int, b: Double) => Some(Constant(a == b))
      case ("==", a: Long, b: Int) => Some(Constant(a == b))
      case ("==", a: Long, b: Long) => Some(Constant(a == b))
      case ("==", a: Long, b: Double) => Some(Constant(a == b))
      case ("==", a: Double, b: Int) => Some(Constant(a == b))
      case ("==", a: Double, b: Long) => Some(Constant(a == b))
      case ("==", a: Double, b: Double) => Some(Constant(a == b))

      case ("!=", a: Int, b: Int) => Some(Constant(a != b))
      case ("!=", a: Int, b: Long) => Some(Constant(a != b))
      case ("!=", a: Int, b: Double) => Some(Constant(a != b))
      case ("!=", a: Long, b: Int) => Some(Constant(a != b))
      case ("!=", a: Long, b: Long) => Some(Constant(a != b))
      case ("!=", a: Long, b: Double) => Some(Constant(a != b))
      case ("!=", a: Double, b: Int) => Some(Constant(a != b))
      case ("!=", a: Double, b: Long) => Some(Constant(a != b))
      case ("!=", a: Double, b: Double) => Some(Constant(a != b))

      case (">", a: Int, b: Int) => Some(Constant(a > b))
      case (">", a: Int, b: Long) => Some(Constant(a > b))
      case (">", a: Int, b: Double) => Some(Constant(a > b))
      case (">", a: Long, b: Int) => Some(Constant(a > b))
      case (">", a: Long, b: Long) => Some(Constant(a > b))
      case (">", a: Long, b: Double) => Some(Constant(a > b))
      case (">", a: Double, b: Int) => Some(Constant(a > b))
      case (">", a: Double, b: Long) => Some(Constant(a > b))
      case (">", a: Double, b: Double) => Some(Constant(a > b))

      case ("<=", a: Int, b: Int) => Some(Constant(a <= b))
      case ("<=", a: Int, b: Long) => Some(Constant(a <= b))
      case ("<=", a: Int, b: Double) => Some(Constant(a <= b))
      case ("<=", a: Long, b: Int) => Some(Constant(a <= b))
      case ("<=", a: Long, b: Long) => Some(Constant(a <= b))
      case ("<=", a: Long, b: Double) => Some(Constant(a <= b))
      case ("<=", a: Double, b: Int) => Some(Constant(a <= b))
      case ("<=", a: Double, b: Long) => Some(Constant(a <= b))
      case ("<=", a: Double, b: Double) => Some(Constant(a <= b))

      case (">=", a: Int, b: Int) => Some(Constant(a >= b))
      case (">=", a: Int, b: Long) => Some(Constant(a >= b))
      case (">=", a: Int, b: Double) => Some(Constant(a >= b))
      case (">=", a: Long, b: Int) => Some(Constant(a >= b))
      case (">=", a: Long, b: Long) => Some(Constant(a >= b))
      case (">=", a: Long, b: Double) => Some(Constant(a >= b))
      case (">=", a: Double, b: Int) => Some(Constant(a >= b))
      case (">=", a: Double, b: Long) => Some(Constant(a >= b))
      case (">=", a: Double, b: Double) => Some(Constant(a >= b))

      case ("&&", a: Boolean, b: Boolean) => Some(Constant(a && b))
      case ("||", a: Boolean, b: Boolean) => Some(Constant(a || b))

      case ("Min", a: Int, b: Int) => Some(Constant(min(a, b)))
      case ("Min", a: Long, b: Long) => Some(Constant(min(a, b)))
      case ("Min", a: Double, b: Double) => Some(Constant(min(a, b)))

      case ("Max", a: Int, b: Int) => Some(Constant(max(a, b)))
      case ("Max", a: Long, b: Long) => Some(Constant(max(a, b)))
      case ("Max", a: Double, b: Double) => Some(Constant(max(a, b)))

      case ("Substring", a: String, b: Int) => Some(Constant(a.substring(b)))

      case _ => abort(s"Unsupported $funcName[$aValue, $bValue]", true)
    }
    ret
  }
  
  final class MaterializeOp2AuxGen(opSym: TypeSymbol,
                                   nTpe: Type,
                                   s1Tpe: Type,
                                   s2Tpe: Type) {
    def usingFuncName[N, T1, T2]: Tree = {
      val funcName = extractSingletonValue[N](nTpe).asInstanceOf[String]
      val aValue = extractSingletonValue[T1](s1Tpe)
      val bValue = extractSingletonValue[T2](s2Tpe)

      val (outWideTpe, outWideLiteral, outTypeName, outTpe, outTree) = op2Calc(funcName,aValue,bValue) match {
        case Some(Constant(t)) =>  constantTypeAndValueOf(t)
      }

      val appliedTpe = tq"$opSym[$nTpe, $s1Tpe, $s2Tpe]"
      val genTree = q"""
        new $appliedTpe {
          type OutWide = $outWideTpe
          type Out = $outTpe
          type $outTypeName = $outTpe
          val value: $outTpe = $outTree
          val valueWide: $outWideTpe = $outWideLiteral
        }
      """
//      print(genTree)
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

  //copied from Shapeless
  import scala.annotation.tailrec
  def mkNatTpt(i: Int): Tree = {
    val succSym = typeOf[shapeless.Succ[_]].typeConstructor.typeSymbol
    val _0Sym = typeOf[shapeless._0].typeSymbol

    @tailrec
    def loop(i: Int, acc: Tree): Tree = {
      if (i == 0) acc
      else loop(i - 1, AppliedTypeTree(Ident(succSym), List(acc)))
    }

    loop(i, Ident(_0Sym))
  }

  //copied from Shapeless
  def mkNatTpe(i: Int): Type = {
    val succTpe = typeOf[shapeless.Succ[_]].typeConstructor
    val _0Tpe = typeOf[shapeless._0]

    @tailrec
    def loop(i: Int, acc: Type): Type = {
      if (i == 0) acc
      else loop(i - 1, appliedType(succTpe, acc))
    }

    loop(i, _0Tpe)
  }

  //copied from Shapeless
  def mkNatValue(i: Int): Tree =
  q""" new ${mkNatTpt(i)} """
  ///////////////////////////////////////////////////////////////////////////////////////////
}
