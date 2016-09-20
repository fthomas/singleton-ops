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

        ////////////////////////////////////////////////////////////////////////
        // Operational Function
        ////////////////////////////////////////////////////////////////////////
        case TypeRef(_, sym, args) if sym == symbolOf[OpMacro[_,_,_,_]] =>
          val funcName = unapply(args.head)
          val aValue = unapply(args(1))
          (funcName, aValue) match {
            case (Some(Constant("ITE")), Some(Constant(cond : Boolean))) => //Special control case: ITE (If-Then-Else)
              if (cond)
                unapply(args(2)) //true (then) part of the IF
              else
                unapply(args(3)) //false (else) part of the IF
            case (Some(Constant("While")), Some(Constant(cond : Boolean))) => //Special control case: While
                var repeat = cond
                while (repeat) {
                  unapply(args(2)) //executing body
                  repeat = unapply(args(1)) match { //reevaluate condition
                    case Some(Constant(updatedCond : Boolean)) => updatedCond
                    case _ => false
                  }
                }
                unapply(args(3)) //return value of loop
            case _ => //regular cases
              val bValue = unapply(args(2))
              val cValue = unapply(args(3))
              (funcName, aValue, bValue, cValue) match {
                case (Some(Constant(f : String)), Some(Constant(a)), Some(Constant(b)), Some(Constant(c))) => Some(opCalc(f, a, b, c))
                case _ => None
              }
          }
        ////////////////////////////////////////////////////////////////////////

        case TypeRef(_, sym, _) if sym.isAliasType => unapply(tp.dealias)
        case TypeRef(pre, sym, Nil) =>
          unapply(sym.info asSeenFrom (pre, sym.owner))
        case SingleType(pre, sym) =>
          unapply(sym.info asSeenFrom (pre, sym.owner))
        case ConstantType(t) => Some(t)
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

  object VariablesHolder {
    val map = collection.mutable.Map[String, Constant]()
    def setVar(name : String, value : Constant) : Constant = {
      map(name) = value
      value
    }
    def getVar(name : String) : Constant = {
      map(name)
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Three operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOpGen[F, N, S1, S2, S3](
      implicit ev0: c.WeakTypeTag[F],
      evn: c.WeakTypeTag[N],
      evs1: c.WeakTypeTag[S1],
      evs2: c.WeakTypeTag[S2],
      evs3: c.WeakTypeTag[S3]
                                     ): MaterializeOpAuxGen =
    new MaterializeOpAuxGen(
      symbolOf[F],
      weakTypeOf[N],
      weakTypeOf[S1],
      weakTypeOf[S2],
      weakTypeOf[S3]
    )

  def opCalc[T1, T2, T3](funcName : String, aValue : T1, bValue : T2, cValue : T3) : Constant = {
    import scala.math._
    val ret = (funcName, aValue, bValue, cValue) match {
      case ("Id",           a: Int, _, _)     => Constant(a)
      case ("Id",           a: Long, _, _)    => Constant(a)
      case ("Id",           a: Double, _, _)  => Constant(a)
      case ("Id",           a: String, _, _)  => Constant(a)
      case ("Id",           a: Boolean, _, _) => Constant(a)
      case ("ToNat",        a: Int, _, _)     => Constant(a)
      case ("ToInt",        a: Int, _, _)     => Constant(a.toInt)
      case ("ToInt",        a: Long, _, _)    => Constant(a.toInt)
      case ("ToInt",        a: Double, _, _)  => Constant(a.toInt)
      case ("ToLong",       a: Int, _, _)     => Constant(a.toLong)
      case ("ToLong",       a: Long, _, _)    => Constant(a.toLong)
      case ("ToLong",       a: Double, _, _)  => Constant(a.toLong)
      case ("ToDouble",     a: Int, _, _)     => Constant(a.toDouble)
      case ("ToDouble",     a: Long, _, _)    => Constant(a.toDouble)
      case ("ToDouble",     a: Double, _, _)  => Constant(a.toDouble)
      case ("Negate",       a: Int, _, _)     => Constant(-a)
      case ("Negate",       a: Long, _, _)    => Constant(-a)
      case ("Negate",       a: Double, _, _)  => Constant(-a)
      case ("Abs",          a: Int, _, _)     => Constant(abs(a))
      case ("Abs",          a: Long, _, _)    => Constant(abs(a))
      case ("Abs",          a: Double, _, _)  => Constant(abs(a))
      case ("Reverse",      a: String, _, _)  => Constant(a.reverse)
      case ("!",            a: Boolean, _, _) => Constant(!a)
      case ("Require",      a: Boolean, _, _) =>
        if (!a)
          abort(s"Cannot prove requirement Require[...]", false)
        else
          Constant(a)
      case ("==>",        _,          b,          _)       => Constant(b)
      case ("SV",         a: String,  b,          _)       => VariablesHolder.setVar(a, Constant(b))
      case ("GV",         a: String,  _,          _)       => VariablesHolder.getVar(a)

      case ("+",          a: Int,     b: Int,     _)     => Constant(a + b)
      case ("+",          a: Int,     b: Long,    _)    => Constant(a + b)
      case ("+",          a: Int,     b: Double,  _)  => Constant(a + b)
      case ("+",          a: Long,    b: Int,     _)     => Constant(a + b)
      case ("+",          a: Long,    b: Long,    _)    => Constant(a + b)
      case ("+",          a: Long,    b: Double,  _)  => Constant(a + b)
      case ("+",          a: Double,  b: Int,     _)     => Constant(a + b)
      case ("+",          a: Double,  b: Long,    _)    => Constant(a + b)
      case ("+",          a: Double,  b: Double,  _)  => Constant(a + b)
      case ("+",          a: String,  b: String,  _)  => Constant(a + b)

      case ("-",          a: Int,     b: Int,     _)     => Constant(a - b)
      case ("-",          a: Int,     b: Long,    _)    => Constant(a - b)
      case ("-",          a: Int,     b: Double,  _)  => Constant(a - b)
      case ("-",          a: Long,    b: Int,     _)     => Constant(a - b)
      case ("-",          a: Long,    b: Long,    _)    => Constant(a - b)
      case ("-",          a: Long,    b: Double,  _)  => Constant(a - b)
      case ("-",          a: Double,  b: Int,     _)     => Constant(a - b)
      case ("-",          a: Double,  b: Long,    _)    => Constant(a - b)
      case ("-",          a: Double,  b: Double,  _)  => Constant(a - b)

      case ("*",          a: Int,     b: Int,     _)     => Constant(a * b)
      case ("*",          a: Int,     b: Long,    _)    => Constant(a * b)
      case ("*",          a: Int,     b: Double,  _)  => Constant(a * b)
      case ("*",          a: Long,    b: Int,     _)     => Constant(a * b)
      case ("*",          a: Long,    b: Long,    _)    => Constant(a * b)
      case ("*",          a: Long,    b: Double,  _)  => Constant(a * b)
      case ("*",          a: Double,  b: Int,     _)     => Constant(a * b)
      case ("*",          a: Double,  b: Long,    _)    => Constant(a * b)
      case ("*",          a: Double,  b: Double,  _)  => Constant(a * b)

      case ("/",          a: Int,     b: Int,     _)     => Constant(a / b)
      case ("/",          a: Int,     b: Long,    _)    => Constant(a / b)
      case ("/",          a: Int,     b: Double,  _)  => Constant(a / b)
      case ("/",          a: Long,    b: Int,     _)     => Constant(a / b)
      case ("/",          a: Long,    b: Long,    _)    => Constant(a / b)
      case ("/",          a: Long,    b: Double,  _)  => Constant(a / b)
      case ("/",          a: Double,  b: Int,     _)     => Constant(a / b)
      case ("/",          a: Double,  b: Long,    _)    => Constant(a / b)
      case ("/",          a: Double,  b: Double,  _)  => Constant(a / b)

      case ("%",          a: Int,     b: Int,     _)     => Constant(a % b)
      case ("%",          a: Int,     b: Long,    _)    => Constant(a % b)
      case ("%",          a: Int,     b: Double,  _)  => Constant(a % b)
      case ("%",          a: Long,    b: Int,     _)     => Constant(a % b)
      case ("%",          a: Long,    b: Long,    _)    => Constant(a % b)
      case ("%",          a: Long,    b: Double,  _)  => Constant(a % b)
      case ("%",          a: Double,  b: Int,     _)     => Constant(a % b)
      case ("%",          a: Double,  b: Long,    _)    => Constant(a % b)
      case ("%",          a: Double,  b: Double,  _)  => Constant(a % b)

      case ("<",          a: Int,     b: Int,     _)     => Constant(a < b)
      case ("<",          a: Int,     b: Long,    _)    => Constant(a < b)
      case ("<",          a: Int,     b: Double,  _)  => Constant(a < b)
      case ("<",          a: Long,    b: Int,     _)     => Constant(a < b)
      case ("<",          a: Long,    b: Long,    _)    => Constant(a < b)
      case ("<",          a: Long,    b: Double,  _)  => Constant(a < b)
      case ("<",          a: Double,  b: Int,     _)     => Constant(a < b)
      case ("<",          a: Double,  b: Long,    _)    => Constant(a < b)
      case ("<",          a: Double,  b: Double,  _)  => Constant(a < b)

      case ("==",         a: Int,     b: Int,     _)     => Constant(a == b)
      case ("==",         a: Int,     b: Long,    _)    => Constant(a == b)
      case ("==",         a: Int,     b: Double,  _)  => Constant(a == b)
      case ("==",         a: Long,    b: Int,     _)     => Constant(a == b)
      case ("==",         a: Long,    b: Long,    _)    => Constant(a == b)
      case ("==",         a: Long,    b: Double,  _)  => Constant(a == b)
      case ("==",         a: Double,  b: Int,     _)     => Constant(a == b)
      case ("==",         a: Double,  b: Long,    _)    => Constant(a == b)
      case ("==",         a: Double,  b: Double,  _)  => Constant(a == b)
      case ("==",         a: Boolean, b: Boolean, _) => Constant(a == b)

      case ("!=",         a: Int,     b: Int,     _)     => Constant(a != b)
      case ("!=",         a: Int,     b: Long,    _)    => Constant(a != b)
      case ("!=",         a: Int,     b: Double,  _)  => Constant(a != b)
      case ("!=",         a: Long,    b: Int,     _)     => Constant(a != b)
      case ("!=",         a: Long,    b: Long,    _)    => Constant(a != b)
      case ("!=",         a: Long,    b: Double,  _)  => Constant(a != b)
      case ("!=",         a: Double,  b: Int,     _)     => Constant(a != b)
      case ("!=",         a: Double,  b: Long,    _)    => Constant(a != b)
      case ("!=",         a: Double,  b: Double,  _)  => Constant(a != b)
      case ("!=",         a: Boolean, b: Boolean, _) => Constant(a != b)

      case (">",          a: Int,     b: Int,     _)     => Constant(a > b)
      case (">",          a: Int,     b: Long,    _)    => Constant(a > b)
      case (">",          a: Int,     b: Double,  _)  => Constant(a > b)
      case (">",          a: Long,    b: Int,     _)     => Constant(a > b)
      case (">",          a: Long,    b: Long,    _)    => Constant(a > b)
      case (">",          a: Long,    b: Double,  _)  => Constant(a > b)
      case (">",          a: Double,  b: Int,     _)     => Constant(a > b)
      case (">",          a: Double,  b: Long,    _)    => Constant(a > b)
      case (">",          a: Double,  b: Double,  _)  => Constant(a > b)

      case ("<=",         a: Int,     b: Int,     _)     => Constant(a <= b)
      case ("<=",         a: Int,     b: Long,    _)    => Constant(a <= b)
      case ("<=",         a: Int,     b: Double,  _)  => Constant(a <= b)
      case ("<=",         a: Long,    b: Int,     _)     => Constant(a <= b)
      case ("<=",         a: Long,    b: Long,    _)    => Constant(a <= b)
      case ("<=",         a: Long,    b: Double,  _)  => Constant(a <= b)
      case ("<=",         a: Double,  b: Int,     _)     => Constant(a <= b)
      case ("<=",         a: Double,  b: Long,    _)    => Constant(a <= b)
      case ("<=",         a: Double,  b: Double,  _)  => Constant(a <= b)

      case (">=",         a: Int,     b: Int,     _)     => Constant(a >= b)
      case (">=",         a: Int,     b: Long,    _)    => Constant(a >= b)
      case (">=",         a: Int,     b: Double,  _)  => Constant(a >= b)
      case (">=",         a: Long,    b: Int,     _)     => Constant(a >= b)
      case (">=",         a: Long,    b: Long,    _)    => Constant(a >= b)
      case (">=",         a: Long,    b: Double,  _)  => Constant(a >= b)
      case (">=",         a: Double,  b: Int,     _)     => Constant(a >= b)
      case (">=",         a: Double,  b: Long,    _)    => Constant(a >= b)
      case (">=",         a: Double,  b: Double,  _)  => Constant(a >= b)

      case ("&&",         a: Boolean, b: Boolean, _) => Constant(a && b)
      case ("||",         a: Boolean, b: Boolean, _) => Constant(a || b)

      case ("Min",        a: Int,     b: Int,     _)     => Constant(min(a, b))
      case ("Min",        a: Long,    b: Long,    _)    => Constant(min(a, b))
      case ("Min",        a: Double,  b: Double,  _)  => Constant(min(a, b))

      case ("Max",        a: Int,     b: Int,     _)     => Constant(max(a, b))
      case ("Max",        a: Long,    b: Long,    _)    => Constant(max(a, b))
      case ("Max",        a: Double,  b: Double,  _)  => Constant(max(a, b))

      case ("Substring",  a: String,  b: Int,     _)     => Constant(a.substring(b))

      case _ => abort(s"Unsupported $funcName[$aValue, $bValue, $cValue]", true)
    }
    ret
  }

  final class MaterializeOpAuxGen(opSym: TypeSymbol,
                                  nTpe: Type,
                                  s1Tpe: Type,
                                  s2Tpe: Type,
                                  s3Tpe: Type
                                  ) {
    def usingFuncName[N, T1, T2, T3]: Tree = {
      val funcName = extractSingletonValue[N](nTpe).asInstanceOf[String]
      val aValue = extractSingletonValue[T1](s1Tpe)
      val bValue = extractSingletonValue[T2](s2Tpe)
      val cValue = extractSingletonValue[T3](s3Tpe)

      val (outWideTpe, outWideLiteral, outTypeName, outTpe, outTree) = (funcName, opCalc(funcName,aValue,bValue,cValue)) match {
        case ("ToNat", Constant(t : Int)) => constantTypeAndValueOfNat(t)
        case (_, Constant(t)) =>  constantTypeAndValueOf(t)
      }

      val appliedTpe = tq"$opSym[$nTpe, $s1Tpe, $s2Tpe, $s3Tpe]"
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
