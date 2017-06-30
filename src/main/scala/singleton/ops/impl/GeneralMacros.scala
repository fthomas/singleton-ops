package singleton.ops.impl
import macrocompat.bundle
import shapeless.Nat

import scala.reflect.macros.whitebox

@bundle
trait GeneralMacros {
  val c: whitebox.Context

  import c.universe._

  ////////////////////////////////////////////////////////////////////
  // Code thanks to Shapeless
  // https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/lazy.scala
  ////////////////////////////////////////////////////////////////////
  def setAnnotation(msg: String)(implicit annotatedSym : TypeSymbol): Unit = {
    import c.internal._
    import decorators._
    val tree0 =
      c.typecheck(
        q"""
          new _root_.scala.annotation.implicitNotFound("dummy")
        """,
        silent = false
      )

    class SubstMessage extends Transformer {
      val global = c.universe.asInstanceOf[scala.tools.nsc.Global]

      override def transform(tree: Tree): Tree = {
        super.transform {
          tree match {
            case Literal(Constant("dummy")) => Literal(Constant(msg))
            case t => t
          }
        }
      }
    }

    val tree = new SubstMessage().transform(tree0)

    annotatedSym.setAnnotations(Annotation(tree))
    ()
  }
  ////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////
  // Code thanks to Paul Phillips
  // https://github.com/paulp/psply/blob/master/src/main/scala/PsplyMacros.scala
  ////////////////////////////////////////////////////////////////////
  sealed trait Calc {
    type T0
    val t : T0
  }
  object Calc {
    def apply[T](_t : T) = new Calc{type T0 = T; val t = _t}
    def unapply(arg: Calc): Option[arg.T0] = Some(arg.t)
  }
  case class CalcLit[T](t: T) extends Calc {type T0 = T}
  case class CalcNLit[T](t: T) extends Calc {type T0 = T}
  object CalcNLChar extends CalcNLit('\u0001')
  object CalcNLInt extends CalcNLit(1)
  object CalcNLLong extends CalcNLit(1L)
  object CalcNLFloat extends CalcNLit(1.0f)
  object CalcNLDouble extends CalcNLit(1.0)
  object CalcNLString extends CalcNLit("1")
  object CalcNLBoolean extends CalcNLit(true)
//  case class CalcUnknown(t: Type) extends Calc {type T0 = Type}

//  case class Calc(const : Constant)
//  object Calc {
//    def apply[T <: Any](t : T) = new Calc(Constant(t))
//    def unapply(calc: Calc): Option[Constant] = Some(calc.const)
//  }

  import scala.reflect.internal.SymbolTable

  /** Typecheck singleton types so as to obtain indirectly
    *  available known-at-compile-time values.
    */
  object Const {
    def unapply(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      val g = c.universe.asInstanceOf[SymbolTable]
      implicit def fixSymbolOps(sym: Symbol): g.Symbol = sym.asInstanceOf[g.Symbol]

      //      print(tp + " RAW " + showRaw(tp))
      tp match {
        case tp @ ExistentialType(_, _) => unapply(tp.underlying)
        case TypeBounds(lo, hi) => unapply(hi)
        case RefinedType(parents, scope) =>
          parents.iterator map unapply collectFirst { case Some(x) => x }
        case NullaryMethodType(tpe) => unapply(tpe)

        ////////////////////////////////////////////////////////////////////////
        // Non-literal values
        ////////////////////////////////////////////////////////////////////////
        case TypeRef(_, sym, _) if sym == symbolOf[Char] => Some(CalcNLChar)
        case TypeRef(_, sym, _) if sym == symbolOf[Int] => Some(CalcNLInt)
        case TypeRef(_, sym, _) if sym == symbolOf[Long] => Some(CalcNLLong)
        case TypeRef(_, sym, _) if sym == symbolOf[Float] => Some(CalcNLFloat)
        case TypeRef(_, sym, _) if sym == symbolOf[Double] => Some(CalcNLDouble)
        case TypeRef(_, sym, _) if sym == symbolOf[String] => Some(CalcNLString)
        case TypeRef(_, sym, _) if sym == symbolOf[Boolean] => Some(CalcNLBoolean)
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // For Shapeless Nat
        ////////////////////////////////////////////////////////////////////////
        case TypeRef(_, sym, args) if sym == symbolOf[shapeless.Succ[_]] =>
          val next = unapply(args.head)
          next match {
            case Some(CalcLit(t: Int)) => Some(CalcLit(t + 1))
            case _ => None
          }
        case TypeRef(_, sym, _) if sym == symbolOf[shapeless._0] =>
          Some(CalcLit(0))
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // Operational Function
        ////////////////////////////////////////////////////////////////////////
        case TypeRef(_, sym, args) if sym == symbolOf[OpMacro[_,_,_,_]] =>
          val funcName = unapply(args.head)

          //If function is set/get variable we keep the original string,
          //otherwise we get the variable's value
          val aValue = unapply(args(1))
          val retVal = (funcName, aValue) match {
            case (Some(CalcLit("AcceptNonLiteral")), _) => //Getting non-literal type
              aValue
            case (Some(CalcLit("IsNonLiteral")), _) => //Looking for non literals
              aValue match {
                case None => Some(CalcLit(true)) //Unknown value
                case Some(CalcNLit(t)) => Some(CalcLit(true)) //non-literal type (e.g., Int, Long,...)
                case _ => Some(CalcLit(false))
              }
            case (Some(CalcLit("ITE")), Some(CalcLit(cond : Boolean))) => //Special control case: ITE (If-Then-Else)
              if (cond)
                unapply(args(2)) //true (then) part of the IF
              else
                unapply(args(3)) //false (else) part of the IF
            case (Some(CalcLit("ITE")), Some(CalcNLBoolean)) => //Non-literal condition will return non-literal type
              val bValue = unapply(args(2)) //used to take the type from the true clause of the If
              bValue match {
                case Some(Calc(t)) => Some(CalcNLit(t)) //forcing non-literal type
                case _ => None
              }
            case _ => //regular cases
              val bValue = unapply(args(2))
              val cValue = unapply(args(3))
              (funcName, aValue, bValue, cValue) match {
                case (Some(CalcLit("Require")), Some(CalcLit(a)), Some(CalcLit(b)), None) =>
                  implicit val annotatedSym : TypeSymbol = args(3).typeSymbol.asType
                  Some(opCalc("Require", a, b, c))
                case (Some(CalcLit(f : String)), Some(Calc(a)), Some(Calc(b)), Some(Calc(c))) =>
                  val calc = opCalc(f, a, b, c)
                  (aValue, bValue, cValue) match {
                    //All parameters are literals, so the result is a literal
                    case (Some(CalcLit(a0)), Some(CalcLit(b0)), Some(CalcLit(c0))) => Some(calc)
                    //One or more of the paramters is non-literal, so the result is non-literal
                    case _ => Some(CalcNLit(calc.t))
                  }
                case _ => None
              }
          }
          retVal

        ////////////////////////////////////////////////////////////////////////

        case TypeRef(_, sym, _) if sym.isAliasType => unapply(tp.dealias)
        case TypeRef(pre, sym, Nil) =>
          unapply(sym.info asSeenFrom (pre, sym.owner))
        case SingleType(pre, sym) =>
          unapply(sym.info asSeenFrom (pre, sym.owner))
        case ConstantType(Constant(t)) => Some(CalcLit(t))
        case _ =>
//          print("Exhausted search at: " + showRaw(tp))
          None
      }
    }
  }
  ////////////////////////////////////////////////////////////////////

  def abort(msg: String)(implicit annotatedSym : TypeSymbol): Nothing = {
    setAnnotation(msg)
    c.abort(c.enclosingPosition, msg)
  }

  def constantTreeOf[T](t : T) : Tree = Literal(Constant(t))

  def constantTypeOf[T](t: T) : Type = c.internal.constantType(Constant(t))

  def constantTypeAndValueOf[T](t: T)(implicit annotatedSym : TypeSymbol): (Type, Tree, TypeName, Type, Tree) ={
    val outWideLiteral = Literal(Constant(t))
    val (outWideType, outTypeName) = t match {
      case tt : Nat => (typeOf[Nat], TypeName("OutNat"))
      case tt : Char => (typeOf[Char], TypeName("OutChar"))
      case tt : Int => (typeOf[Int], TypeName("OutInt"))
      case tt : Long => (typeOf[Long], TypeName("OutLong"))
      case tt : Float => (typeOf[Float], TypeName("OutFloat"))
      case tt : Double => (typeOf[Double], TypeName("OutDouble"))
      case tt : String => (typeOf[String], TypeName("OutString"))
      case tt : Boolean => (typeOf[Boolean], TypeName("OutBoolean"))
      case _ => abort(s"Unsupported type $t")
    }
    (outWideType, outWideLiteral, outTypeName, constantTypeOf(t), constantTreeOf(t))
  }

  def constantTypeAndValueOfNat(t: Int): (Type, Tree, TypeName, Type, Tree) ={
    val outWideType = typeOf[Int]
    val outWideLiteral = Literal(Constant(t))
    val outTypeName = TypeName("OutNat")
    (outWideType, outWideLiteral, outTypeName, mkNatTpe(t), q"new ${mkNatTpt(t)}")
  }

  def typeAndValueOfNLit[T](calc : CalcNLit[T])(implicit wtt : c.TypeTag[Option[T]]) :
  (Type, Tree, TypeName, Type, Tree) ={
    val retType = typeOf[Option[T]]
    (retType, q"None", TypeName("IgnoreMe"), retType, q"None")
  }

  def extractionFailed(tpe: Type)(implicit annotatedSym : TypeSymbol) = {
    val msg = s"Cannot extract value from $tpe\n" + "showRaw==> " + showRaw(tpe)
    abort(msg)
  }

  def extractSingletonValue(tpe: Type)(implicit annotatedSym : TypeSymbol): Calc = {
    val value = Const.unapply(tpe) match {
      case None => extractionFailed(tpe)
      case Some(calc) => calc
    }
    value
  }

  def extractValueFromOpTree(opTree : c.Tree)(implicit annotatedSym : TypeSymbol) : Option[Constant] = {
    def outFindCond(elem : c.Tree) : Boolean = elem match {
      case q"val value : $typeTree = $valueTree" => true
      case _ => false
    }
    def getOut(opClsBlk : List[c.Tree]) : Option[Constant] = opClsBlk.find(outFindCond) match {
      case Some(q"val value : $typeTree = $valueTree") =>
        valueTree match {
          case Literal(const) => Some(const)
          case _ => None
        }
      case _ => None
    }

    opTree match {
      case q"""{
        $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents { $self => ..$opClsBlk }
        $expr(...$exprss)
      }""" => getOut(opClsBlk)
      case _ => None
    }
  }

  def evalTyped[T](expr: c.Expr[T]): T =
    c.eval(c.Expr[T](c.untypecheck(expr.tree)))

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Three operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOpGen[F, N](implicit ev0: c.WeakTypeTag[F], evn: c.WeakTypeTag[N]): MaterializeOpAuxGen =
    new MaterializeOpAuxGen(weakTypeOf[F], weakTypeOf[N])

  def opCalc[T1, T2, T3](funcName : String, aValue : T1, bValue : T2, cValue : T3)(implicit annotatedSym : TypeSymbol) : Calc = {
    import scala.math._
    val ret = (funcName, aValue, bValue, cValue) match {
      case ("Id",         a: Char, _, _)              => CalcLit(a)
      case ("Id",         a: Int, _, _)               => CalcLit(a)
      case ("Id",         a: Long, _, _)              => CalcLit(a)
      case ("Id",         a: Float, _, _)             => CalcLit(a)
      case ("Id",         a: Double, _, _)            => CalcLit(a)
      case ("Id",         a: String, _, _)            => CalcLit(a)
      case ("Id",         a: Boolean, _, _)           => CalcLit(a)

      case ("ToNat",      a: Char, _, _)              => CalcLit(a.toInt)
      case ("ToNat",      a: Int, _, _)               => CalcLit(a.toInt)
      case ("ToNat",      a: Long, _, _)              => CalcLit(a.toInt)
      case ("ToNat",      a: Float, _, _)             => CalcLit(a.toInt)
      case ("ToNat",      a: Double, _, _)            => CalcLit(a.toInt)
      case ("ToNat",      a: String, _, _)            => CalcLit(a.toInt)

      case ("ToChar",     a: Char, _, _)              => CalcLit(a.toChar)
      case ("ToChar",     a: Int, _, _)               => CalcLit(a.toChar)
      case ("ToChar",     a: Long, _, _)              => CalcLit(a.toChar)
      case ("ToChar",     a: Float, _, _)             => CalcLit(a.toChar)
      case ("ToChar",     a: Double, _, _)            => CalcLit(a.toChar)

      case ("ToInt",      a: Char, _, _)              => CalcLit(a.toInt)
      case ("ToInt",      a: Int, _, _)               => CalcLit(a.toInt)
      case ("ToInt",      a: Long, _, _)              => CalcLit(a.toInt)
      case ("ToInt",      a: Float, _, _)             => CalcLit(a.toInt)
      case ("ToInt",      a: Double, _, _)            => CalcLit(a.toInt)
      case ("ToInt",      a: String, _, _)            => CalcLit(a.toInt)

      case ("ToLong",     a: Char, _, _)              => CalcLit(a.toLong)
      case ("ToLong",     a: Int, _, _)               => CalcLit(a.toLong)
      case ("ToLong",     a: Long, _, _)              => CalcLit(a.toLong)
      case ("ToLong",     a: Float, _, _)             => CalcLit(a.toLong)
      case ("ToLong",     a: Double, _, _)            => CalcLit(a.toLong)
      case ("ToLong",     a: String, _, _)            => CalcLit(a.toLong)

      case ("ToFloat",    a: Char, _, _)              => CalcLit(a.toFloat)
      case ("ToFloat",    a: Int, _, _)               => CalcLit(a.toFloat)
      case ("ToFloat",    a: Long, _, _)              => CalcLit(a.toFloat)
      case ("ToFloat",    a: Float, _, _)             => CalcLit(a.toFloat)
      case ("ToFloat",    a: Double, _, _)            => CalcLit(a.toFloat)
      case ("ToFloat",    a: String, _, _)            => CalcLit(a.toFloat)

      case ("ToDouble",   a: Char, _, _)              => CalcLit(a.toDouble)
      case ("ToDouble",   a: Int, _, _)               => CalcLit(a.toDouble)
      case ("ToDouble",   a: Long, _, _)              => CalcLit(a.toDouble)
      case ("ToDouble",   a: Float, _, _)             => CalcLit(a.toDouble)
      case ("ToDouble",   a: Double, _, _)            => CalcLit(a.toDouble)
      case ("ToDouble",   a: String, _, _)            => CalcLit(a.toDouble)

      case ("ToString",   a: Char, _, _)              => CalcLit(a.toString)
      case ("ToString",   a: Int, _, _)               => CalcLit(a.toString)
      case ("ToString",   a: Long, _, _)              => CalcLit(a.toString)
      case ("ToString",   a: Float, _, _)             => CalcLit(a.toString)
      case ("ToString",   a: Double, _, _)            => CalcLit(a.toString)
      case ("ToString",   a: String, _, _)            => CalcLit(a.toString)
      case ("ToString",   a: Boolean, _, _)           => CalcLit(a.toString)

      case ("IsNat",      a: Char, _, _)              => CalcLit(false)
      case ("IsNat",      a: Int, _, _)               => CalcLit(a >= 0)
      case ("IsNat",      a: Long, _, _)              => CalcLit(false)
      case ("IsNat",      a: Float, _, _)             => CalcLit(false)
      case ("IsNat",      a: Double, _, _)            => CalcLit(false)
      case ("IsNat",      a: String, _, _)            => CalcLit(false)
      case ("IsNat",      a: Boolean, _, _)           => CalcLit(false)

      case ("IsChar",     a: Char, _, _)              => CalcLit(true)
      case ("IsChar",     a: Int, _, _)               => CalcLit(false)
      case ("IsChar",     a: Long, _, _)              => CalcLit(false)
      case ("IsChar",     a: Float, _, _)             => CalcLit(false)
      case ("IsChar",     a: Double, _, _)            => CalcLit(false)
      case ("IsChar",     a: String, _, _)            => CalcLit(false)
      case ("IsChar",     a: Boolean, _, _)           => CalcLit(false)

      case ("IsInt",      a: Char, _, _)              => CalcLit(false)
      case ("IsInt",      a: Int, _, _)               => CalcLit(true)
      case ("IsInt",      a: Long, _, _)              => CalcLit(false)
      case ("IsInt",      a: Float, _, _)             => CalcLit(false)
      case ("IsInt",      a: Double, _, _)            => CalcLit(false)
      case ("IsInt",      a: String, _, _)            => CalcLit(false)
      case ("IsInt",      a: Boolean, _, _)           => CalcLit(false)

      case ("IsLong",     a: Char, _, _)              => CalcLit(false)
      case ("IsLong",     a: Int, _, _)               => CalcLit(false)
      case ("IsLong",     a: Long, _, _)              => CalcLit(true)
      case ("IsLong",     a: Float, _, _)             => CalcLit(false)
      case ("IsLong",     a: Double, _, _)            => CalcLit(false)
      case ("IsLong",     a: String, _, _)            => CalcLit(false)
      case ("IsLong",     a: Boolean, _, _)           => CalcLit(false)

      case ("IsFloat",    a: Char, _, _)              => CalcLit(false)
      case ("IsFloat",    a: Int, _, _)               => CalcLit(false)
      case ("IsFloat",    a: Long, _, _)              => CalcLit(false)
      case ("IsFloat",    a: Float, _, _)             => CalcLit(true)
      case ("IsFloat",    a: Double, _, _)            => CalcLit(false)
      case ("IsFloat",    a: String, _, _)            => CalcLit(false)
      case ("IsFloat",    a: Boolean, _, _)           => CalcLit(false)

      case ("IsDouble",   a: Char, _, _)              => CalcLit(false)
      case ("IsDouble",   a: Int, _, _)               => CalcLit(false)
      case ("IsDouble",   a: Long, _, _)              => CalcLit(false)
      case ("IsDouble",   a: Float, _, _)             => CalcLit(false)
      case ("IsDouble",   a: Double, _, _)            => CalcLit(true)
      case ("IsDouble",   a: String, _, _)            => CalcLit(false)
      case ("IsDouble",   a: Boolean, _, _)           => CalcLit(false)

      case ("IsString",   a: Char, _, _)              => CalcLit(false)
      case ("IsString",   a: Int, _, _)               => CalcLit(false)
      case ("IsString",   a: Long, _, _)              => CalcLit(false)
      case ("IsString",   a: Float, _, _)             => CalcLit(false)
      case ("IsString",   a: Double, _, _)            => CalcLit(false)
      case ("IsString",   a: String, _, _)            => CalcLit(true)
      case ("IsString",   a: Boolean, _, _)           => CalcLit(false)

      case ("IsBoolean",  a: Char, _, _)              => CalcLit(false)
      case ("IsBoolean",  a: Int, _, _)               => CalcLit(false)
      case ("IsBoolean",  a: Long, _, _)              => CalcLit(false)
      case ("IsBoolean",  a: Float, _, _)             => CalcLit(false)
      case ("IsBoolean",  a: Double, _, _)            => CalcLit(false)
      case ("IsBoolean",  a: String, _, _)            => CalcLit(false)
      case ("IsBoolean",  a: Boolean, _, _)           => CalcLit(true)

      case ("Negate",     a: Char, _, _)              => CalcLit(-a)
      case ("Negate",     a: Int, _, _)               => CalcLit(-a)
      case ("Negate",     a: Long, _, _)              => CalcLit(-a)
      case ("Negate",     a: Float, _, _)             => CalcLit(-a)
      case ("Negate",     a: Double, _, _)            => CalcLit(-a)

      case ("Abs",        a: Int, _, _)               => CalcLit(abs(a))
      case ("Abs",        a: Long, _, _)              => CalcLit(abs(a))
      case ("Abs",        a: Float, _, _)             => CalcLit(abs(a))
      case ("Abs",        a: Double, _, _)            => CalcLit(abs(a))

      case ("NumberOfLeadingZeros", a: Int, _, _)     => CalcLit(java.lang.Integer.numberOfLeadingZeros(a))
      case ("NumberOfLeadingZeros", a: Long, _, _)    => CalcLit(java.lang.Long.numberOfLeadingZeros(a))

      case ("Floor",      a: Float, _, _)             => CalcLit(floor(a.toDouble))
      case ("Floor",      a: Double, _, _)            => CalcLit(floor(a))

      case ("Ceil",       a: Float, _, _)             => CalcLit(ceil(a.toDouble))
      case ("Ceil",       a: Double, _, _)            => CalcLit(ceil(a))

      case ("Round",      a: Float, _, _)             => CalcLit(round(a))
      case ("Round",      a: Double, _, _)            => CalcLit(round(a))

      case ("Sin",        a: Float, _, _)             => CalcLit(sin(a.toDouble))
      case ("Sin",        a: Double, _, _)            => CalcLit(sin(a))

      case ("Cos",        a: Float, _, _)             => CalcLit(cos(a.toDouble))
      case ("Cos",        a: Double, _, _)            => CalcLit(cos(a))

      case ("Tan",        a: Float, _, _)             => CalcLit(tan(a.toDouble))
      case ("Tan",        a: Double, _, _)            => CalcLit(tan(a))

      case ("Sqrt",       a: Float, _, _)             => CalcLit(sqrt(a.toDouble))
      case ("Sqrt",       a: Double, _, _)            => CalcLit(sqrt(a))

      case ("Log",        a: Float, _, _)             => CalcLit(log(a.toDouble))
      case ("Log",        a: Double, _, _)            => CalcLit(log(a))

      case ("Log10",      a: Float, _, _)             => CalcLit(log10(a.toDouble))
      case ("Log10",      a: Double, _, _)            => CalcLit(log10(a))

      case ("Reverse",    a: String, _, _)            => CalcLit(a.reverse)
      case ("!",          a: Boolean, _, _)           => CalcLit(!a)
      case ("Require",    a: Boolean, b: String, _)   =>
        if (!a)
          abort(b)
        else
          CalcLit(a)
      case ("==>",        _,          b,          _)  => CalcLit(b)

      case ("+",          a: Char,    b: Char,    _)  => CalcLit(a + b)
      case ("+",          a: Char,    b: Int,     _)  => CalcLit(a + b)
      case ("+",          a: Char,    b: Long,    _)  => CalcLit(a + b)
      case ("+",          a: Char,    b: Float,   _)  => CalcLit(a + b)
      case ("+",          a: Char,    b: Double,  _)  => CalcLit(a + b)
      case ("+",          a: Int,     b: Char,    _)  => CalcLit(a + b)
      case ("+",          a: Int,     b: Int,     _)  => CalcLit(a + b)
      case ("+",          a: Int,     b: Long,    _)  => CalcLit(a + b)
      case ("+",          a: Int,     b: Float,   _)  => CalcLit(a + b)
      case ("+",          a: Int,     b: Double,  _)  => CalcLit(a + b)
      case ("+",          a: Long,    b: Char,    _)  => CalcLit(a + b)
      case ("+",          a: Long,    b: Int,     _)  => CalcLit(a + b)
      case ("+",          a: Long,    b: Long,    _)  => CalcLit(a + b)
      case ("+",          a: Long,    b: Float,   _)  => CalcLit(a + b)
      case ("+",          a: Long,    b: Double,  _)  => CalcLit(a + b)
      case ("+",          a: Float,   b: Char,    _)  => CalcLit(a + b)
      case ("+",          a: Float,   b: Int,     _)  => CalcLit(a + b)
      case ("+",          a: Float,   b: Long,    _)  => CalcLit(a + b)
      case ("+",          a: Float,   b: Float,   _)  => CalcLit(a + b)
      case ("+",          a: Float,   b: Double,  _)  => CalcLit(a + b)
      case ("+",          a: Double,  b: Char,    _)  => CalcLit(a + b)
      case ("+",          a: Double,  b: Int,     _)  => CalcLit(a + b)
      case ("+",          a: Double,  b: Long,    _)  => CalcLit(a + b)
      case ("+",          a: Double,  b: Float,   _)  => CalcLit(a + b)
      case ("+",          a: Double,  b: Double,  _)  => CalcLit(a + b)
      case ("+",          a: String,  b: String,  _)  => CalcLit(a + b) //Concat

      case ("-",          a: Char,    b: Char,    _)  => CalcLit(a - b)
      case ("-",          a: Char,    b: Int,     _)  => CalcLit(a - b)
      case ("-",          a: Char,    b: Long,    _)  => CalcLit(a - b)
      case ("-",          a: Char,    b: Float,   _)  => CalcLit(a - b)
      case ("-",          a: Char,    b: Double,  _)  => CalcLit(a - b)
      case ("-",          a: Int,     b: Char,    _)  => CalcLit(a - b)
      case ("-",          a: Int,     b: Int,     _)  => CalcLit(a - b)
      case ("-",          a: Int,     b: Long,    _)  => CalcLit(a - b)
      case ("-",          a: Int,     b: Float,   _)  => CalcLit(a - b)
      case ("-",          a: Int,     b: Double,  _)  => CalcLit(a - b)
      case ("-",          a: Long,    b: Char,    _)  => CalcLit(a - b)
      case ("-",          a: Long,    b: Int,     _)  => CalcLit(a - b)
      case ("-",          a: Long,    b: Long,    _)  => CalcLit(a - b)
      case ("-",          a: Long,    b: Float,   _)  => CalcLit(a - b)
      case ("-",          a: Long,    b: Double,  _)  => CalcLit(a - b)
      case ("-",          a: Float,   b: Char,    _)  => CalcLit(a - b)
      case ("-",          a: Float,   b: Int,     _)  => CalcLit(a - b)
      case ("-",          a: Float,   b: Long,    _)  => CalcLit(a - b)
      case ("-",          a: Float,   b: Float,   _)  => CalcLit(a - b)
      case ("-",          a: Float,   b: Double,  _)  => CalcLit(a - b)
      case ("-",          a: Double,  b: Char,    _)  => CalcLit(a - b)
      case ("-",          a: Double,  b: Int,     _)  => CalcLit(a - b)
      case ("-",          a: Double,  b: Long,    _)  => CalcLit(a - b)
      case ("-",          a: Double,  b: Float,   _)  => CalcLit(a - b)
      case ("-",          a: Double,  b: Double,  _)  => CalcLit(a - b)

      case ("*",          a: Char,    b: Char,    _)  => CalcLit(a * b)
      case ("*",          a: Char,    b: Int,     _)  => CalcLit(a * b)
      case ("*",          a: Char,    b: Long,    _)  => CalcLit(a * b)
      case ("*",          a: Char,    b: Float,   _)  => CalcLit(a * b)
      case ("*",          a: Char,    b: Double,  _)  => CalcLit(a * b)
      case ("*",          a: Int,     b: Char,    _)  => CalcLit(a * b)
      case ("*",          a: Int,     b: Int,     _)  => CalcLit(a * b)
      case ("*",          a: Int,     b: Long,    _)  => CalcLit(a * b)
      case ("*",          a: Int,     b: Float,   _)  => CalcLit(a * b)
      case ("*",          a: Int,     b: Double,  _)  => CalcLit(a * b)
      case ("*",          a: Long,    b: Char,    _)  => CalcLit(a * b)
      case ("*",          a: Long,    b: Int,     _)  => CalcLit(a * b)
      case ("*",          a: Long,    b: Long,    _)  => CalcLit(a * b)
      case ("*",          a: Long,    b: Float,   _)  => CalcLit(a * b)
      case ("*",          a: Long,    b: Double,  _)  => CalcLit(a * b)
      case ("*",          a: Float,   b: Char,    _)  => CalcLit(a * b)
      case ("*",          a: Float,   b: Int,     _)  => CalcLit(a * b)
      case ("*",          a: Float,   b: Long,    _)  => CalcLit(a * b)
      case ("*",          a: Float,   b: Float,   _)  => CalcLit(a * b)
      case ("*",          a: Float,   b: Double,  _)  => CalcLit(a * b)
      case ("*",          a: Double,  b: Char,    _)  => CalcLit(a * b)
      case ("*",          a: Double,  b: Int,     _)  => CalcLit(a * b)
      case ("*",          a: Double,  b: Long,    _)  => CalcLit(a * b)
      case ("*",          a: Double,  b: Float,   _)  => CalcLit(a * b)
      case ("*",          a: Double,  b: Double,  _)  => CalcLit(a * b)

      case ("/",          a: Char,    b: Char,    _)  => CalcLit(a / b)
      case ("/",          a: Char,    b: Int,     _)  => CalcLit(a / b)
      case ("/",          a: Char,    b: Long,    _)  => CalcLit(a / b)
      case ("/",          a: Char,    b: Float,   _)  => CalcLit(a / b)
      case ("/",          a: Char,    b: Double,  _)  => CalcLit(a / b)
      case ("/",          a: Int,     b: Char,    _)  => CalcLit(a / b)
      case ("/",          a: Int,     b: Int,     _)  => CalcLit(a / b)
      case ("/",          a: Int,     b: Long,    _)  => CalcLit(a / b)
      case ("/",          a: Int,     b: Float,   _)  => CalcLit(a / b)
      case ("/",          a: Int,     b: Double,  _)  => CalcLit(a / b)
      case ("/",          a: Long,    b: Char,    _)  => CalcLit(a / b)
      case ("/",          a: Long,    b: Int,     _)  => CalcLit(a / b)
      case ("/",          a: Long,    b: Long,    _)  => CalcLit(a / b)
      case ("/",          a: Long,    b: Float,   _)  => CalcLit(a / b)
      case ("/",          a: Long,    b: Double,  _)  => CalcLit(a / b)
      case ("/",          a: Float,   b: Char,    _)  => CalcLit(a / b)
      case ("/",          a: Float,   b: Int,     _)  => CalcLit(a / b)
      case ("/",          a: Float,   b: Long,    _)  => CalcLit(a / b)
      case ("/",          a: Float,   b: Float,   _)  => CalcLit(a / b)
      case ("/",          a: Float,   b: Double,  _)  => CalcLit(a / b)
      case ("/",          a: Double,  b: Char,    _)  => CalcLit(a / b)
      case ("/",          a: Double,  b: Int,     _)  => CalcLit(a / b)
      case ("/",          a: Double,  b: Long,    _)  => CalcLit(a / b)
      case ("/",          a: Double,  b: Float,   _)  => CalcLit(a / b)
      case ("/",          a: Double,  b: Double,  _)  => CalcLit(a / b)

      case ("%",          a: Char,    b: Char,    _)  => CalcLit(a % b)
      case ("%",          a: Char,    b: Int,     _)  => CalcLit(a % b)
      case ("%",          a: Char,    b: Long,    _)  => CalcLit(a % b)
      case ("%",          a: Char,    b: Float,   _)  => CalcLit(a % b)
      case ("%",          a: Char,    b: Double,  _)  => CalcLit(a % b)
      case ("%",          a: Int,     b: Char,    _)  => CalcLit(a % b)
      case ("%",          a: Int,     b: Int,     _)  => CalcLit(a % b)
      case ("%",          a: Int,     b: Long,    _)  => CalcLit(a % b)
      case ("%",          a: Int,     b: Float,   _)  => CalcLit(a % b)
      case ("%",          a: Int,     b: Double,  _)  => CalcLit(a % b)
      case ("%",          a: Long,    b: Char,    _)  => CalcLit(a % b)
      case ("%",          a: Long,    b: Int,     _)  => CalcLit(a % b)
      case ("%",          a: Long,    b: Long,    _)  => CalcLit(a % b)
      case ("%",          a: Long,    b: Float,   _)  => CalcLit(a % b)
      case ("%",          a: Long,    b: Double,  _)  => CalcLit(a % b)
      case ("%",          a: Float,   b: Char,    _)  => CalcLit(a % b)
      case ("%",          a: Float,   b: Int,     _)  => CalcLit(a % b)
      case ("%",          a: Float,   b: Long,    _)  => CalcLit(a % b)
      case ("%",          a: Float,   b: Float,   _)  => CalcLit(a % b)
      case ("%",          a: Float,   b: Double,  _)  => CalcLit(a % b)
      case ("%",          a: Double,  b: Char,    _)  => CalcLit(a % b)
      case ("%",          a: Double,  b: Int,     _)  => CalcLit(a % b)
      case ("%",          a: Double,  b: Long,    _)  => CalcLit(a % b)
      case ("%",          a: Double,  b: Float,   _)  => CalcLit(a % b)
      case ("%",          a: Double,  b: Double,  _)  => CalcLit(a % b)

      case ("Pow",        a: Float,   b: Float,   _)  => CalcLit(pow(a.toDouble,b.toDouble))
      case ("Pow",        a: Float,   b: Double,  _)  => CalcLit(pow(a.toDouble,b.toDouble))
      case ("Pow",        a: Double,  b: Float,   _)  => CalcLit(pow(a.toDouble,b.toDouble))
      case ("Pow",        a: Double,  b: Double,  _)  => CalcLit(pow(a.toDouble,b.toDouble))

      case ("==",         a: Char,    b: Char,    _)  => CalcLit(a == b)
      case ("==",         a: Char,    b: Int,     _)  => CalcLit(a == b)
      case ("==",         a: Char,    b: Long,    _)  => CalcLit(a == b)
      case ("==",         a: Char,    b: Float,   _)  => CalcLit(a == b)
      case ("==",         a: Char,    b: Double,  _)  => CalcLit(a == b)
      case ("==",         a: Int,     b: Char,    _)  => CalcLit(a == b)
      case ("==",         a: Int,     b: Int,     _)  => CalcLit(a == b)
      case ("==",         a: Int,     b: Long,    _)  => CalcLit(a == b)
      case ("==",         a: Int,     b: Float,   _)  => CalcLit(a == b)
      case ("==",         a: Int,     b: Double,  _)  => CalcLit(a == b)
      case ("==",         a: Long,    b: Char,    _)  => CalcLit(a == b)
      case ("==",         a: Long,    b: Int,     _)  => CalcLit(a == b)
      case ("==",         a: Long,    b: Long,    _)  => CalcLit(a == b)
      case ("==",         a: Long,    b: Float,   _)  => CalcLit(a == b)
      case ("==",         a: Long,    b: Double,  _)  => CalcLit(a == b)
      case ("==",         a: Float,   b: Char,    _)  => CalcLit(a == b)
      case ("==",         a: Float,   b: Int,     _)  => CalcLit(a == b)
      case ("==",         a: Float,   b: Long,    _)  => CalcLit(a == b)
      case ("==",         a: Float,   b: Float,   _)  => CalcLit(a == b)
      case ("==",         a: Float,   b: Double,  _)  => CalcLit(a == b)
      case ("==",         a: Double,  b: Char,    _)  => CalcLit(a == b)
      case ("==",         a: Double,  b: Int,     _)  => CalcLit(a == b)
      case ("==",         a: Double,  b: Long,    _)  => CalcLit(a == b)
      case ("==",         a: Double,  b: Float,   _)  => CalcLit(a == b)
      case ("==",         a: Double,  b: Double,  _)  => CalcLit(a == b)
      case ("==",         a: String,  b: String,  _)  => CalcLit(a == b)
      case ("==",         a: Boolean, b: Boolean, _)  => CalcLit(a == b)

      case ("!=",         a: Char,    b: Char,    _)  => CalcLit(a != b)
      case ("!=",         a: Char,    b: Int,     _)  => CalcLit(a != b)
      case ("!=",         a: Char,    b: Long,    _)  => CalcLit(a != b)
      case ("!=",         a: Char,    b: Float,   _)  => CalcLit(a != b)
      case ("!=",         a: Char,    b: Double,  _)  => CalcLit(a != b)
      case ("!=",         a: Int,     b: Char,    _)  => CalcLit(a != b)
      case ("!=",         a: Int,     b: Int,     _)  => CalcLit(a != b)
      case ("!=",         a: Int,     b: Long,    _)  => CalcLit(a != b)
      case ("!=",         a: Int,     b: Float,   _)  => CalcLit(a != b)
      case ("!=",         a: Int,     b: Double,  _)  => CalcLit(a != b)
      case ("!=",         a: Long,    b: Char,    _)  => CalcLit(a != b)
      case ("!=",         a: Long,    b: Int,     _)  => CalcLit(a != b)
      case ("!=",         a: Long,    b: Long,    _)  => CalcLit(a != b)
      case ("!=",         a: Long,    b: Float,   _)  => CalcLit(a != b)
      case ("!=",         a: Long,    b: Double,  _)  => CalcLit(a != b)
      case ("!=",         a: Float,   b: Char,    _)  => CalcLit(a != b)
      case ("!=",         a: Float,   b: Int,     _)  => CalcLit(a != b)
      case ("!=",         a: Float,   b: Long,    _)  => CalcLit(a != b)
      case ("!=",         a: Float,   b: Float,   _)  => CalcLit(a != b)
      case ("!=",         a: Float,   b: Double,  _)  => CalcLit(a != b)
      case ("!=",         a: Double,  b: Char,    _)  => CalcLit(a != b)
      case ("!=",         a: Double,  b: Int,     _)  => CalcLit(a != b)
      case ("!=",         a: Double,  b: Long,    _)  => CalcLit(a != b)
      case ("!=",         a: Double,  b: Float,   _)  => CalcLit(a != b)
      case ("!=",         a: Double,  b: Double,  _)  => CalcLit(a != b)
      case ("!=",         a: String,  b: String,  _)  => CalcLit(a != b)
      case ("!=",         a: Boolean, b: Boolean, _)  => CalcLit(a != b)

      case ("<",          a: Char,    b: Char,    _)  => CalcLit(a < b)
      case ("<",          a: Char,    b: Int,     _)  => CalcLit(a < b)
      case ("<",          a: Char,    b: Long,    _)  => CalcLit(a < b)
      case ("<",          a: Char,    b: Float,   _)  => CalcLit(a < b)
      case ("<",          a: Char,    b: Double,  _)  => CalcLit(a < b)
      case ("<",          a: Int,     b: Char,    _)  => CalcLit(a < b)
      case ("<",          a: Int,     b: Int,     _)  => CalcLit(a < b)
      case ("<",          a: Int,     b: Long,    _)  => CalcLit(a < b)
      case ("<",          a: Int,     b: Float,   _)  => CalcLit(a < b)
      case ("<",          a: Int,     b: Double,  _)  => CalcLit(a < b)
      case ("<",          a: Long,    b: Char,    _)  => CalcLit(a < b)
      case ("<",          a: Long,    b: Int,     _)  => CalcLit(a < b)
      case ("<",          a: Long,    b: Long,    _)  => CalcLit(a < b)
      case ("<",          a: Long,    b: Float,   _)  => CalcLit(a < b)
      case ("<",          a: Long,    b: Double,  _)  => CalcLit(a < b)
      case ("<",          a: Float,   b: Char,    _)  => CalcLit(a < b)
      case ("<",          a: Float,   b: Int,     _)  => CalcLit(a < b)
      case ("<",          a: Float,   b: Long,    _)  => CalcLit(a < b)
      case ("<",          a: Float,   b: Float,   _)  => CalcLit(a < b)
      case ("<",          a: Float,   b: Double,  _)  => CalcLit(a < b)
      case ("<",          a: Double,  b: Char,    _)  => CalcLit(a < b)
      case ("<",          a: Double,  b: Int,     _)  => CalcLit(a < b)
      case ("<",          a: Double,  b: Long,    _)  => CalcLit(a < b)
      case ("<",          a: Double,  b: Float,   _)  => CalcLit(a < b)
      case ("<",          a: Double,  b: Double,  _)  => CalcLit(a < b)

      case (">",          a: Char,    b: Char,    _)  => CalcLit(a > b)
      case (">",          a: Char,    b: Int,     _)  => CalcLit(a > b)
      case (">",          a: Char,    b: Long,    _)  => CalcLit(a > b)
      case (">",          a: Char,    b: Float,   _)  => CalcLit(a > b)
      case (">",          a: Char,    b: Double,  _)  => CalcLit(a > b)
      case (">",          a: Int,     b: Char,    _)  => CalcLit(a > b)
      case (">",          a: Int,     b: Int,     _)  => CalcLit(a > b)
      case (">",          a: Int,     b: Long,    _)  => CalcLit(a > b)
      case (">",          a: Int,     b: Float,   _)  => CalcLit(a > b)
      case (">",          a: Int,     b: Double,  _)  => CalcLit(a > b)
      case (">",          a: Long,    b: Char,    _)  => CalcLit(a > b)
      case (">",          a: Long,    b: Int,     _)  => CalcLit(a > b)
      case (">",          a: Long,    b: Long,    _)  => CalcLit(a > b)
      case (">",          a: Long,    b: Float,   _)  => CalcLit(a > b)
      case (">",          a: Long,    b: Double,  _)  => CalcLit(a > b)
      case (">",          a: Float,   b: Char,    _)  => CalcLit(a > b)
      case (">",          a: Float,   b: Int,     _)  => CalcLit(a > b)
      case (">",          a: Float,   b: Long,    _)  => CalcLit(a > b)
      case (">",          a: Float,   b: Float,   _)  => CalcLit(a > b)
      case (">",          a: Float,   b: Double,  _)  => CalcLit(a > b)
      case (">",          a: Double,  b: Char,    _)  => CalcLit(a > b)
      case (">",          a: Double,  b: Int,     _)  => CalcLit(a > b)
      case (">",          a: Double,  b: Long,    _)  => CalcLit(a > b)
      case (">",          a: Double,  b: Float,   _)  => CalcLit(a > b)
      case (">",          a: Double,  b: Double,  _)  => CalcLit(a > b)

      case ("<=",         a: Char,    b: Char,    _)  => CalcLit(a <= b)
      case ("<=",         a: Char,    b: Int,     _)  => CalcLit(a <= b)
      case ("<=",         a: Char,    b: Long,    _)  => CalcLit(a <= b)
      case ("<=",         a: Char,    b: Float,   _)  => CalcLit(a <= b)
      case ("<=",         a: Char,    b: Double,  _)  => CalcLit(a <= b)
      case ("<=",         a: Int,     b: Char,    _)  => CalcLit(a <= b)
      case ("<=",         a: Int,     b: Int,     _)  => CalcLit(a <= b)
      case ("<=",         a: Int,     b: Long,    _)  => CalcLit(a <= b)
      case ("<=",         a: Int,     b: Float,   _)  => CalcLit(a <= b)
      case ("<=",         a: Int,     b: Double,  _)  => CalcLit(a <= b)
      case ("<=",         a: Long,    b: Char,    _)  => CalcLit(a <= b)
      case ("<=",         a: Long,    b: Int,     _)  => CalcLit(a <= b)
      case ("<=",         a: Long,    b: Long,    _)  => CalcLit(a <= b)
      case ("<=",         a: Long,    b: Float,   _)  => CalcLit(a <= b)
      case ("<=",         a: Long,    b: Double,  _)  => CalcLit(a <= b)
      case ("<=",         a: Float,   b: Char,    _)  => CalcLit(a <= b)
      case ("<=",         a: Float,   b: Int,     _)  => CalcLit(a <= b)
      case ("<=",         a: Float,   b: Long,    _)  => CalcLit(a <= b)
      case ("<=",         a: Float,   b: Float,   _)  => CalcLit(a <= b)
      case ("<=",         a: Float,   b: Double,  _)  => CalcLit(a <= b)
      case ("<=",         a: Double,  b: Char,    _)  => CalcLit(a <= b)
      case ("<=",         a: Double,  b: Int,     _)  => CalcLit(a <= b)
      case ("<=",         a: Double,  b: Long,    _)  => CalcLit(a <= b)
      case ("<=",         a: Double,  b: Float,   _)  => CalcLit(a <= b)
      case ("<=",         a: Double,  b: Double,  _)  => CalcLit(a <= b)

      case (">=",         a: Char,    b: Char,    _)  => CalcLit(a >= b)
      case (">=",         a: Char,    b: Int,     _)  => CalcLit(a >= b)
      case (">=",         a: Char,    b: Long,    _)  => CalcLit(a >= b)
      case (">=",         a: Char,    b: Float,   _)  => CalcLit(a >= b)
      case (">=",         a: Char,    b: Double,  _)  => CalcLit(a >= b)
      case (">=",         a: Int,     b: Char,    _)  => CalcLit(a >= b)
      case (">=",         a: Int,     b: Int,     _)  => CalcLit(a >= b)
      case (">=",         a: Int,     b: Long,    _)  => CalcLit(a >= b)
      case (">=",         a: Int,     b: Float,   _)  => CalcLit(a >= b)
      case (">=",         a: Int,     b: Double,  _)  => CalcLit(a >= b)
      case (">=",         a: Long,    b: Char,    _)  => CalcLit(a >= b)
      case (">=",         a: Long,    b: Int,     _)  => CalcLit(a >= b)
      case (">=",         a: Long,    b: Long,    _)  => CalcLit(a >= b)
      case (">=",         a: Long,    b: Float,   _)  => CalcLit(a >= b)
      case (">=",         a: Long,    b: Double,  _)  => CalcLit(a >= b)
      case (">=",         a: Float,   b: Char,    _)  => CalcLit(a >= b)
      case (">=",         a: Float,   b: Int,     _)  => CalcLit(a >= b)
      case (">=",         a: Float,   b: Long,    _)  => CalcLit(a >= b)
      case (">=",         a: Float,   b: Float,   _)  => CalcLit(a >= b)
      case (">=",         a: Float,   b: Double,  _)  => CalcLit(a >= b)
      case (">=",         a: Double,  b: Char,    _)  => CalcLit(a >= b)
      case (">=",         a: Double,  b: Int,     _)  => CalcLit(a >= b)
      case (">=",         a: Double,  b: Long,    _)  => CalcLit(a >= b)
      case (">=",         a: Double,  b: Float,   _)  => CalcLit(a >= b)
      case (">=",         a: Double,  b: Double,  _)  => CalcLit(a >= b)

      case ("&&",         a: Boolean, b: Boolean, _)  => CalcLit(a && b)
      case ("||",         a: Boolean, b: Boolean, _)  => CalcLit(a || b)

      case ("Min",        a: Int,     b: Int,     _)  => CalcLit(min(a, b))
      case ("Min",        a: Long,    b: Long,    _)  => CalcLit(min(a, b))
      case ("Min",        a: Float,   b: Float,   _)  => CalcLit(min(a, b))
      case ("Min",        a: Double,  b: Double,  _)  => CalcLit(min(a, b))

      case ("Max",        a: Int,     b: Int,     _)  => CalcLit(max(a, b))
      case ("Max",        a: Long,    b: Long,    _)  => CalcLit(max(a, b))
      case ("Max",        a: Float,   b: Float,   _)  => CalcLit(max(a, b))
      case ("Max",        a: Double,  b: Double,  _)  => CalcLit(max(a, b))

      case ("Substring",  a: String,  b: Int,     _)  => CalcLit(a.substring(b))
      case ("Length",     a: String,  _,          _)  => CalcLit(a.length)
      case ("CharAt",     a: String,  b: Int,     _)  => CalcLit(a.charAt(b))

      case _ => abort(s"Unsupported $funcName[$aValue, $bValue, $cValue]")
    }
    ret
  }

  final class MaterializeOpAuxGen(opTpe: Type, nTpe: Type) {
    def usingFuncName : Tree = {
      implicit val annotatedSym : TypeSymbol = symbolOf[OpMacro[_,_,_,_]]
      val funcName = extractSingletonValue(nTpe)
      val aValue = extractSingletonValue(opTpe)

      val (outWideTpe, outWideLiteral, outTypeName, outTpe, outTree) = (funcName, aValue) match {
        case (CalcLit("ToNat"), CalcLit(t : Int)) => constantTypeAndValueOfNat(t)
        case (CalcLit(s : String), CalcLit(t)) =>  constantTypeAndValueOf(t)
        case (CalcLit("AcceptNonLiteral"), CalcNLit(t)) => typeAndValueOfNLit(CalcNLit(t))
        case _ => extractionFailed(opTpe)
      }

      val genTree = q"""
      new $opTpe {
        type OutWide = $outWideTpe
        type Out = $outTpe
        type Value = $outTpe
        type $outTypeName = $outTpe
        val value: $outTpe = $outTree
        final val isLiteral = true
        val valueWide: $outWideTpe = $outWideLiteral
      }
      """

//      print(genTree)
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////
  // Checked TwoFace
  ///////////////////////////////////////////////////////////////////////////////////////////
  def CheckedImplMaterializer[T, Param, Chk](implicit t : c.WeakTypeTag[T], param : c.WeakTypeTag[Param], chk : c.WeakTypeTag[Chk]) :
  CheckedImplMaterializer[T, Param, Chk] = new CheckedImplMaterializer[T, Param, Chk](weakTypeOf[T], weakTypeOf[Param], symbolOf[Chk])

  final class CheckedImplMaterializer[T, Param, Chk](tTpe : Type, paramTpe : Type, chkSym : TypeSymbol) {
    def newChecked(paramNum : Int, valueTree : c.Tree)(implicit annotatedSym : TypeSymbol) : c.Tree = {
      paramNum match {
        case 0 => q"new $chkSym[$tTpe]($valueTree)"
        case 1 => q"new $chkSym[$tTpe,$paramTpe]($valueTree)"
        case _ =>
          abort("Unsupported number of parameters")
      }
    }
    def impl(paramNum : Int, vc : c.Tree, vm : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
      val msgValue = extractValueFromOpTree(vm) match {
        case Some(Constant(s : String)) => s
        case _ => abort("Invalid error message:\n" + showRaw(vm))
      }

      extractValueFromOpTree(vc) match {
        case Some(Constant(true)) =>                  //condition given to macro must be true
        case Some(Constant(false)) => abort(msgValue) //otherwise the given error message is set as the abort message
        case _ => abort("Unable to retrieve compile-time value:\n" + showRaw(vm))
      }

      val chkTerm = TermName(chkSym.name.toString)
      val tValue = extractSingletonValue(tTpe) match {
        case CalcLit(t) => t
        case _ => abort("Unable to retrieve compile-time value:\n" + showRaw(tTpe))
      }
      val tValueTree = constantTreeOf(tValue)
      val genTree = newChecked(paramNum, tValueTree)
      genTree
    }
    def unsafe(paramNum : Int, valueTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
      val genTree = newChecked(paramNum, valueTree)
      genTree
    }
    def unsafeTF(paramNum : Int, valueTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
      val genTree = newChecked(paramNum, q"$valueTree.getValue")
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
