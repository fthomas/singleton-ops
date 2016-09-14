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
      case TypeRef(_, sym, args) if sym == symbolOf[shapeless.Succ[_]] =>
        val next = unapply(args.head)
        next match {
          case Some(Constant(t : Int)) => Some(Constant(t+1))
          case _ => None
        }
      case TypeRef(_, sym, _) if sym == symbolOf[shapeless._0] => Some(Constant(0))
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

  def constantTypeAndValueOf[T](t: T)(implicit ev : c.WeakTypeTag[T]): (Type, Type, Tree) =
    (weakTypeOf[T], constantTypeOf(t), Literal(Constant(t)))

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
      val (baseTpe, outTpe, outTree) = (funcName, aValue) match {
        case ("ToInt",    a : Int)      => constantTypeAndValueOf(a.toInt)
        case ("ToInt",    a : Long)     => constantTypeAndValueOf(a.toInt)
        case ("ToInt",    a : Double)   => constantTypeAndValueOf(a.toInt)
        case ("ToLong",   a : Int)      => constantTypeAndValueOf(a.toLong)
        case ("ToLong",   a : Long)     => constantTypeAndValueOf(a.toLong)
        case ("ToLong",   a : Double)   => constantTypeAndValueOf(a.toLong)
        case ("ToDouble", a : Int)      => constantTypeAndValueOf(a.toDouble)
        case ("ToDouble", a : Long)     => constantTypeAndValueOf(a.toDouble)
        case ("ToDouble", a : Double)   => constantTypeAndValueOf(a.toDouble)
        case ("Negate",   a : Int)      => constantTypeAndValueOf(-a)
        case ("Negate",   a : Long)     => constantTypeAndValueOf(-a)
        case ("Negate",   a : Double)   => constantTypeAndValueOf(-a)
        case ("Abs",      a : Int)      => constantTypeAndValueOf(abs(a))
        case ("Abs",      a : Long)     => constantTypeAndValueOf(abs(a))
        case ("Abs",      a : Double)   => constantTypeAndValueOf(abs(a))
        case ("Reverse",  a : String)   => constantTypeAndValueOf(a.reverse)
        case ("!",        a : Boolean)  => constantTypeAndValueOf(!a)
        case ("Require",  a : Boolean)  =>
          if (!a)
            abort(s"Cannot prove requirement Require[...]", true)
          else
            constantTypeAndValueOf(a)
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
      val (baseTpe, outTpe, outTree) = (funcName, aValue, bValue) match {
        case ("+",            a : Int,     b : Int)     => constantTypeAndValueOf(a + b)
        case ("+",            a : Int,     b : Long)    => constantTypeAndValueOf(a + b)
        case ("+",            a : Int,     b : Double)  => constantTypeAndValueOf(a + b)
        case ("+",            a : Long,    b : Int)     => constantTypeAndValueOf(a + b)
        case ("+",            a : Long,    b : Long)    => constantTypeAndValueOf(a + b)
        case ("+",            a : Long,    b : Double)  => constantTypeAndValueOf(a + b)
        case ("+",            a : Double,  b : Int)     => constantTypeAndValueOf(a + b)
        case ("+",            a : Double,  b : Long)    => constantTypeAndValueOf(a + b)
        case ("+",            a : Double,  b : Double)  => constantTypeAndValueOf(a + b)
        case ("+",            a : String,  b : String)  => constantTypeAndValueOf(a + b)

        case ("-",            a : Int,     b : Int)     => constantTypeAndValueOf(a - b)
        case ("-",            a : Int,     b : Long)    => constantTypeAndValueOf(a - b)
        case ("-",            a : Int,     b : Double)  => constantTypeAndValueOf(a - b)
        case ("-",            a : Long,    b : Int)     => constantTypeAndValueOf(a - b)
        case ("-",            a : Long,    b : Long)    => constantTypeAndValueOf(a - b)
        case ("-",            a : Long,    b : Double)  => constantTypeAndValueOf(a - b)
        case ("-",            a : Double,  b : Int)     => constantTypeAndValueOf(a - b)
        case ("-",            a : Double,  b : Long)    => constantTypeAndValueOf(a - b)
        case ("-",            a : Double,  b : Double)  => constantTypeAndValueOf(a - b)

        case ("*",            a : Int,     b : Int)     => constantTypeAndValueOf(a * b)
        case ("*",            a : Int,     b : Double)  => constantTypeAndValueOf(a * b)
        case ("*",            a : Long,    b : Int)     => constantTypeAndValueOf(a * b)
        case ("*",            a : Long,    b : Long)    => constantTypeAndValueOf(a * b)
        case ("*",            a : Long,    b : Double)  => constantTypeAndValueOf(a * b)
        case ("*",            a : Double,  b : Int)     => constantTypeAndValueOf(a * b)
        case ("*",            a : Double,  b : Long)    => constantTypeAndValueOf(a * b)
        case ("*",            a : Double,  b : Double)  => constantTypeAndValueOf(a * b)

        case ("/",            a : Int,     b : Int)     => constantTypeAndValueOf(a / b)
        case ("/",            a : Int,     b : Long)    => constantTypeAndValueOf(a / b)
        case ("/",            a : Int,     b : Double)  => constantTypeAndValueOf(a / b)
        case ("/",            a : Long,    b : Int)     => constantTypeAndValueOf(a / b)
        case ("/",            a : Long,    b : Long)    => constantTypeAndValueOf(a / b)
        case ("/",            a : Long,    b : Double)  => constantTypeAndValueOf(a / b)
        case ("/",            a : Double,  b : Int)     => constantTypeAndValueOf(a / b)
        case ("/",            a : Double,  b : Long)    => constantTypeAndValueOf(a / b)
        case ("/",            a : Double,  b : Double)  => constantTypeAndValueOf(a / b)

        case ("<",            a : Int,     b : Int)     => constantTypeAndValueOf(a < b)
        case ("<",            a : Int,     b : Long)    => constantTypeAndValueOf(a < b)
        case ("<",            a : Int,     b : Double)  => constantTypeAndValueOf(a < b)
        case ("<",            a : Long,    b : Int)     => constantTypeAndValueOf(a < b)
        case ("<",            a : Long,    b : Long)    => constantTypeAndValueOf(a < b)
        case ("<",            a : Long,    b : Double)  => constantTypeAndValueOf(a < b)
        case ("<",            a : Double,  b : Int)     => constantTypeAndValueOf(a < b)
        case ("<",            a : Double,  b : Long)    => constantTypeAndValueOf(a < b)
        case ("<",            a : Double,  b : Double)  => constantTypeAndValueOf(a < b)

        case ("==",           a : Int,     b : Int)    => constantTypeAndValueOf(a == b)
        case ("==",           a : Int,     b : Long)   => constantTypeAndValueOf(a == b)
        case ("==",           a : Int,     b : Double) => constantTypeAndValueOf(a == b)
        case ("==",           a : Long,    b : Int)    => constantTypeAndValueOf(a == b)
        case ("==",           a : Long,    b : Long)   => constantTypeAndValueOf(a == b)
        case ("==",           a : Long,    b : Double) => constantTypeAndValueOf(a == b)
        case ("==",           a : Double,  b : Int)    => constantTypeAndValueOf(a == b)
        case ("==",           a : Double,  b : Long)   => constantTypeAndValueOf(a == b)
        case ("==",           a : Double,  b : Double) => constantTypeAndValueOf(a == b)

        case ("!=",           a : Int,     b : Int)    => constantTypeAndValueOf(a != b)
        case ("!=",           a : Int,     b : Long)   => constantTypeAndValueOf(a != b)
        case ("!=",           a : Int,     b : Double) => constantTypeAndValueOf(a != b)
        case ("!=",           a : Long,    b : Int)    => constantTypeAndValueOf(a != b)
        case ("!=",           a : Long,    b : Long)   => constantTypeAndValueOf(a != b)
        case ("!=",           a : Long,    b : Double) => constantTypeAndValueOf(a != b)
        case ("!=",           a : Double,  b : Int)    => constantTypeAndValueOf(a != b)
        case ("!=",           a : Double,  b : Long)   => constantTypeAndValueOf(a != b)
        case ("!=",           a : Double,  b : Double) => constantTypeAndValueOf(a != b)

        case (">",            a : Int,     b : Int)     => constantTypeAndValueOf(a > b)
        case (">",            a : Int,     b : Long)    => constantTypeAndValueOf(a > b)
        case (">",            a : Int,     b : Double)  => constantTypeAndValueOf(a > b)
        case (">",            a : Long,    b : Int)     => constantTypeAndValueOf(a > b)
        case (">",            a : Long,    b : Long)    => constantTypeAndValueOf(a > b)
        case (">",            a : Long,    b : Double)  => constantTypeAndValueOf(a > b)
        case (">",            a : Double,  b : Int)     => constantTypeAndValueOf(a > b)
        case (">",            a : Double,  b : Long)    => constantTypeAndValueOf(a > b)
        case (">",            a : Double,  b : Double)  => constantTypeAndValueOf(a > b)

        case ("<=",           a : Int,     b : Int)     => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Int,     b : Long)    => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Int,     b : Double)  => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Long,    b : Int)     => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Long,    b : Long)    => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Long,    b : Double)  => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Double,  b : Int)     => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Double,  b : Long)    => constantTypeAndValueOf(a <= b)
        case ("<=",           a : Double,  b : Double)  => constantTypeAndValueOf(a <= b)

        case (">=",           a : Int,     b : Int)     => constantTypeAndValueOf(a >= b)
        case (">=",           a : Int,     b : Long)    => constantTypeAndValueOf(a >= b)
        case (">=",           a : Int,     b : Double)  => constantTypeAndValueOf(a >= b)
        case (">=",           a : Long,    b : Int)     => constantTypeAndValueOf(a >= b)
        case (">=",           a : Long,    b : Long)    => constantTypeAndValueOf(a >= b)
        case (">=",           a : Long,    b : Double)  => constantTypeAndValueOf(a >= b)
        case (">=",           a : Double,  b : Int)     => constantTypeAndValueOf(a >= b)
        case (">=",           a : Double,  b : Long)    => constantTypeAndValueOf(a >= b)
        case (">=",           a : Double,  b : Double)  => constantTypeAndValueOf(a >= b)

        case ("&&",           a : Boolean, b : Boolean) => constantTypeAndValueOf(a && b)
        case ("||",           a : Boolean, b : Boolean) => constantTypeAndValueOf(a || b)

        case ("Min",          a : Int,     b : Int)     => constantTypeAndValueOf(min(a,b))
        case ("Min",          a : Long,    b : Long)    => constantTypeAndValueOf(min(a,b))
        case ("Min",          a : Double,  b : Double)  => constantTypeAndValueOf(min(a,b))

        case ("Max",          a : Int,     b : Int)     => constantTypeAndValueOf(max(a,b))
        case ("Max",          a : Long,    b : Long)    => constantTypeAndValueOf(max(a,b))
        case ("Max",          a : Double,  b : Double)  => constantTypeAndValueOf(max(a,b))
          
        case ("Substring",    a : String,  b : Int)     => constantTypeAndValueOf(a.substring(b))

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


  ///////////////////////////////////////////////////////////////////////////////////////////
  // ToNat Interface
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeToNat[F, T1, S1](
                                       implicit ev0: c.WeakTypeTag[F],
                                       evt1: c.WeakTypeTag[T1],
                                       evs1: c.WeakTypeTag[S1]): MaterializeToNatAux =
    new MaterializeToNatAux(
      symbolOf[F],
      weakTypeOf[T1],
      weakTypeOf[S1])

  final class MaterializeToNatAux(opSym: TypeSymbol,
                                  t1Tpe: Type,
                                  s1Tpe: Type) {
    def usingFuncName[T1] : Tree = {
      val aValue = extractSingletonValue[T1](s1Tpe)

      val natTree : Tree = aValue match {
        case a : Int =>
          if (a >= 0)
            mkNatValue(a)
          else
            abort(s"Unsupported negative integer to convert to Nat", true)
        case _ =>
          abort(s"Unsupported value type to convert to Nat", true)
      }
      val appliedTpe = tq"$opSym[$t1Tpe, $s1Tpe]"
      q"""
        new $appliedTpe {
          val value: Nat = $natTree
        }
      """

    }

    import shapeless._
    import scala.annotation.tailrec
    def mkNatTpt(i: Int): Tree = {
      val succSym = typeOf[Succ[_]].typeConstructor.typeSymbol
      val _0Sym = typeOf[_0].typeSymbol

      @tailrec
      def loop(i: Int, acc: Tree): Tree = {
        if(i == 0) acc
        else loop(i-1, AppliedTypeTree(Ident(succSym), List(acc)))
      }

      loop(i, Ident(_0Sym))
    }

    def mkNatTpe(i: Int): Type = {
      val succTpe = typeOf[Succ[_]].typeConstructor
      val _0Tpe = typeOf[_0]

      @tailrec
      def loop(i: Int, acc: Type): Type = {
        if(i == 0) acc
        else loop(i-1, appliedType(succTpe, acc))
      }

      loop(i, _0Tpe)
    }

    def mkNatValue(i: Int): Tree =
      q""" new ${mkNatTpt(i)} """
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

//  ///////////////////////////////////////////////////////////////////////////////////////////
//  // ToNat Interface
//  ///////////////////////////////////////////////////////////////////////////////////////////
//  def materializeFromNat[F, N](
//                                   implicit ev0: c.WeakTypeTag[F],
//                                   evn: c.WeakTypeTag[N]): MaterializeFromNatAux =
//  new MaterializeFromNatAux(
//    symbolOf[F],
//    weakTypeOf[N])
//
//  final class MaterializeFromNatAux(opSym: TypeSymbol,
//                                    nTpe: Type) {
//
//    import shapeless._
//    object Const {
//      def unapply(tp: Type): Option[Int] = {
////        print(showRaw(tp))
//        tp match {
//          case tp @ ExistentialType(_, _) => unapply(tp.underlying)
//          case TypeBounds(lo, hi) => unapply(hi)
////          case ClassInfoType(parents,scope,sym) => unapply(parents.head)
////          case RefinedType(parents, _) =>
////            parents.iterator map unapply collectFirst { case Some(x) => x }
//          case NullaryMethodType(tpe) => unapply(tpe)
//          case TypeRef(_, sym, args) if sym == symbolOf[Succ[_]] => Some(unapply(args.head).get + 1)
//          case TypeRef(_, sym, _) if sym == symbolOf[_0] => Some(0)
//          case TypeRef(_, sym, _) if sym.isAliasType => unapply(tp.dealias)
//          case TypeRef(pre, sym, Nil) =>
//            unapply(sym.info asSeenFrom (pre, sym.owner))
//          case SingleType(pre, sym) =>
//            unapply(sym.info asSeenFrom (pre, sym.owner))
////          case ConstantType(c) => Some(c)
//          case _ => None
//        }
//      }
//    }
//
//    def extractNatValue(tpe: Type): Int = {
//      def extractionFailed(tpe: Type) = {
//        val msg = s"Cannot extract Nat value from $tpe\n" + "showRaw==> " + showRaw(
//          tpe)
//        abort(msg, true)
//      }
//
//      val value = tpe match {
//        case Const(t : Int) => t
//        case _ => extractionFailed(tpe); 0
//      }
//
//      value.asInstanceOf[Int]
//    }
//
//    def usingFuncName[T1] : Tree = {
//      val nValue = extractSingletonValue(nTpe)
//      val (bbb, outTpe, outTree) = constantTypeAndValueOf(nValue)
//
//      val appliedTpe = tq"$opSym[$nTpe]"
//      q"""
//        new $appliedTpe {
//          type Out = $outTpe
//          val value: $outTpe = $outTree
//        }
//      """
//    }
//
//    import scala.annotation.tailrec
//    def mkNatTpt(i: Int): Tree = {
//      val succSym = typeOf[Succ[_]].typeConstructor.typeSymbol
//      val _0Sym = typeOf[_0].typeSymbol
//
//      @tailrec
//      def loop(i: Int, acc: Tree): Tree = {
//        if(i == 0) acc
//        else loop(i-1, AppliedTypeTree(Ident(succSym), List(acc)))
//      }
//
//      loop(i, Ident(_0Sym))
//    }
//
//    def mkNatTpe(i: Int): Type = {
//      val succTpe = typeOf[Succ[_]].typeConstructor
//      val _0Tpe = typeOf[_0]
//
//      @tailrec
//      def loop(i: Int, acc: Type): Type = {
//        if(i == 0) acc
//        else loop(i-1, appliedType(succTpe, acc))
//      }
//
//      loop(i, _0Tpe)
//    }
//
//    def mkNatValue(i: Int): Tree =
//      q""" new ${mkNatTpt(i)} """
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////
}
