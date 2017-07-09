package singleton.ops.impl
import macrocompat.bundle

import scala.reflect.macros.whitebox
import scala.reflect.macros.TypecheckException
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
  // Calc
  ////////////////////////////////////////////////////////////////////
  sealed trait Calc {
    type T
  }

  object Calc {
    sealed trait Char extends Calc{type T = scala.Char}
    sealed trait Int extends Calc{type T = scala.Int}
    sealed trait Long extends Calc{type T = scala.Long}
    sealed trait Float extends Calc{type T = scala.Float}
    sealed trait Double extends Calc{type T = scala.Double}
    sealed trait String extends Calc{type T = java.lang.String}
    sealed trait Boolean extends Calc{type T = scala.Boolean}
  }

  sealed trait CalcVal extends Calc {
    type V
    val t : V
  }
  object CalcVal {
    implicit val lift = Liftable[CalcVal] { p =>
      p match {
        case c : CalcLit => Literal(Constant(c.t))
        case c : CalcNLit => c.t
        //        case _ => abort("Unsupported lifting for given calculation")
      }
    }
  }

  sealed trait CalcLit extends CalcVal {
    type V = T
  }

  object CalcLit {
    implicit val lift = Liftable[CalcLit] { p => Literal(Constant(p.t)) }
    case class Char(t : scala.Char) extends CalcLit with Calc.Char
    case class Int(t : scala.Int) extends CalcLit with Calc.Int
    case class Long(t : scala.Long) extends CalcLit with Calc.Long
    case class Float(t : scala.Float) extends CalcLit with Calc.Float
    case class Double(t : scala.Double) extends CalcLit with Calc.Double
    case class String(t : java.lang.String) extends CalcLit with Calc.String
    case class Boolean(t : scala.Boolean) extends CalcLit with Calc.Boolean
    def apply[T](t : T)(implicit unsupported : TypeSymbol) = t match {
      case t : scala.Char => Char(t)
      case t : scala.Int => Int(t)
      case t : scala.Long => Long(t)
      case t : scala.Float => Float(t)
      case t : scala.Double => Double(t)
      case t : java.lang.String => String(t)
      case t : scala.Boolean => Boolean(t)
      case _ => abort("Unsupported type")
    }
    def unapply(arg: CalcLit) : Option[arg.T] = Some(arg.t)
  }

  sealed trait CalcType extends Calc
  object CalcType {
    object Char extends CalcType with Calc.Char
    object Int extends CalcType with Calc.Int
    object Long extends CalcType with Calc.Long
    object Float extends CalcType with Calc.Float
    object Double extends CalcType with Calc.Double
    object String extends CalcType with Calc.String
    object Boolean extends CalcType with Calc.Boolean
  }
  sealed trait CalcNLit extends CalcVal {
    type V = Tree
  }
  object CalcNLit {
    implicit val lift = Liftable[CalcNLit] { p => p.t }
    def apply(_t : Tree) = new CalcNLit{val t = _t}
    def unapply(arg: CalcNLit) : Option[Tree] = Some(arg.t)
  }
  case class CalcUnknown(t: Type) extends Calc
  ////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////
  // Code thanks to Paul Phillips
  // https://github.com/paulp/psply/blob/master/src/main/scala/PsplyMacros.scala
  ////////////////////////////////////////////////////////////////////
  import scala.reflect.internal.SymbolTable

  /** Typecheck singleton types so as to obtain indirectly
    *  available known-at-compile-time values.
    */
  object Const {
    ////////////////////////////////////////////////////////////////////////
    // Calculates the integer value of Shapeless Nat
    ////////////////////////////////////////////////////////////////////////
    def calcNat(tp: Type)(implicit annotatedSym : TypeSymbol): CalcLit.Int = {
      tp match {
        case TypeRef(_, sym, args) if sym == symbolOf[shapeless.Succ[_]] =>
          CalcLit.Int(calcNat(args.head).t + 1)
        case TypeRef(_, sym, _) if sym == symbolOf[shapeless._0] =>
          CalcLit.Int(0)
        case _ =>
          abort(s"Given Nat type is defective: $tp, raw: ${showRaw(tp)}")
      }
    }
    ////////////////////////////////////////////////////////////////////////

    def unapplyOpArg(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      unapply(tp) match {
        case Some(t : CalcLit) =>
          Some(t)
        case Some(t : CalcType) =>
          Some(CalcNLit(q"_root_.shapeless.Witness[$tp].value"))
        case _ =>
          Some(CalcUnknown(tp))
      }
    }

    def unapplyOp(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      val args = tp.typeArgs
      val funcName = unapply(args.head) match {
        case (Some(CalcLit.String(f))) => f
        case _ => abort(s"Unexpected bad function name: ${args.head}")
      }

      //If function is set/get variable we keep the original string,
      //otherwise we get the variable's value
      val aValue = unapplyOpArg(args(1))
      val retVal = (funcName, aValue) match {
        case ("IsNonLiteral", _) => //Looking for non literals
          aValue match {
            case Some(t : CalcLit) => Some(CalcLit(false))
            case _ => Some(CalcLit(true)) //non-literal type (e.g., Int, Long,...)
          }
        case ("ITE", Some(CalcLit.Boolean(cond))) => //Special control case: ITE (If-Then-Else)
          if (cond)
            unapplyOpArg(args(2)) //true (then) part of the IF
          else
            unapplyOpArg(args(3)) //false (else) part of the IF
        case ("ITE", Some(CalcNLit(cond))) => //Non-literal condition will return non-literal type
          val thenArg = unapplyOpArg(args(2))
          val elseArg = unapplyOpArg(args(3))
          (thenArg, elseArg) match {
            case (Some(thenArg0 : CalcVal), Some(elseArg0 : CalcVal)) => Some(CalcNLit(q"if ($cond) $thenArg0 else $elseArg0"))
            case _ => None
          }

        case _ => //regular cases
          val bValue = unapplyOpArg(args(2))
          val cValue = unapplyOpArg(args(3))
          (aValue, bValue, cValue) match {
//            case (Some(CalcLit("Require")), Some(CalcLit(a)), Some(CalcLit(b)), None) =>
//              implicit val annotatedSym : TypeSymbol = args(3).typeSymbol.asType
//              Some(opCalc("Require", a, b, c))
            case (Some(a : Calc), Some(b: Calc), Some(c : Calc)) =>
              Some(opCalc(funcName, a, b, c))
            case _ => None
          }
      }
      retVal
    }

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
        case TypeRef(_, sym, _) if sym == symbolOf[Char] => Some(CalcType.Char)
        case TypeRef(_, sym, _) if sym == symbolOf[Int] => Some(CalcType.Int)
        case TypeRef(_, sym, _) if sym == symbolOf[Long] => Some(CalcType.Long)
        case TypeRef(_, sym, _) if sym == symbolOf[Float] => Some(CalcType.Float)
        case TypeRef(_, sym, _) if sym == symbolOf[Double] => Some(CalcType.Double)
        case TypeRef(_, sym, _) if sym == symbolOf[String] => Some(CalcType.String)
        case TypeRef(_, sym, _) if sym == symbolOf[Boolean] => Some(CalcType.Boolean)
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // For Shapeless Nat
        ////////////////////////////////////////////////////////////////////////
        case TypeRef(_, sym, args) if sym == symbolOf[shapeless.Succ[_]] || sym == symbolOf[shapeless._0] =>
          Some(calcNat(tp))
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // Operational Function
        ////////////////////////////////////////////////////////////////////////
        case TypeRef(_, sym, args) if sym == symbolOf[OpMacro[_,_,_,_]] =>
          unapplyOp(tp)
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

  def runtimeTypeOf[T](t : T)(implicit annotatedSym : TypeSymbol) : Type = {
    t match {
      case tt : Char => typeOf[Char]
      case tt : Int => typeOf[Int]
      case tt : Long => typeOf[Long]
      case tt : Float => typeOf[Float]
      case tt : Double => typeOf[Double]
      case tt : String => typeOf[String]
      case tt : Boolean => typeOf[Boolean]
      case _ => abort(s"Unsupported type $t")
    }
  }
  def genOpTreeLit[T](opTpe : Type, t: T)(implicit annotatedSym : TypeSymbol) : Tree = {
    val outWideLiteral = Literal(Constant(t))
    val outWideTpe = runtimeTypeOf(t)
    val outTypeName = TypeName("Out" + outWideTpe.typeSymbol.name.toString)
    val outTpe = constantTypeOf(t)
    val outTree = constantTreeOf(t)
    val dummy = TermName(c.freshName())
    q"""
      new $opTpe {
        final val $dummy = $outWideLiteral
        type OutWide = $outWideTpe
        type Out = $dummy.type
        type Value = Out
        type $outTypeName = Out
        final val value: Value = $outWideLiteral
        final val isLiteral = true
        final val valueWide: $outWideTpe = $outWideLiteral
      }
      """
  }

  def genOpTreeNat(opTpe : Type, t: Int) : Tree = {
    val outWideTpe = typeOf[Int]
    val outWideLiteral = Literal(Constant(t))
    val outTypeName = TypeName("OutNat")
    val outTpe = mkNatTpe(t)
    val outTree = q"new ${mkNatTpt(t)}"
    q"""
      new $opTpe {
        type OutWide = $outWideTpe
        type Out = $outTpe
        type Value = $outTpe
        type $outTypeName = $outTpe
        final val value: $outTpe = $outTree
        final val isLiteral = true
        final val valueWide: $outWideTpe = $outWideLiteral
      }
      """
  }

  def genOpTreeWitness(opTpe : Type, t : Tree)(implicit annotatedSym : TypeSymbol) : Tree = {
    val dummy = TermName(c.freshName())
    q"""
      new $opTpe {
        final val $dummy = $t
        type OutWide = $dummy.type
        type Out = $dummy.type
        type Value = $dummy.type
        final val value: Value = $dummy
        final val isLiteral = false
        final val valueWide: $dummy.type = $dummy
      }
      """
  }

  def genOpTreeNLit[T](opTpe : Type, t : T)(implicit annotatedSym : TypeSymbol) : Tree = {
    val outTpe = runtimeTypeOf(t)
    q"""
      new $opTpe {
        type OutWide = Option[$outTpe]
        type Out = $outTpe
        type Value = Option[$outTpe]
        final val value: Option[$outTpe] = None
        final val isLiteral = false
        final val valueWide: Option[$outTpe] = None
      }
      """
  }

  def genOpTreeUnknown(opTpe : Type, t : Type)(implicit annotatedSym : TypeSymbol) : Tree = {
    q"""
      new $opTpe {
        type OutWide = Option[$t]
        type Out = $t
        type Value = Option[$t]
        final val value: Option[$t] = None
        final val isLiteral = false
        final val valueWide: Option[$t] = None
      }
      """
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
      case q"final val value : $typeTree = $valueTree" => true
      case _ => false
    }
    def getOut(opClsBlk : List[c.Tree]) : Option[Constant] = opClsBlk.find(outFindCond) match {
      case Some(q"final val value : $typeTree = $valueTree") =>
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

  def evalTyped[T](tree: Tree)(implicit annotatedSym : TypeSymbol): Constant = {
    try {
      Constant(c.eval(c.Expr(c.untypecheck(tree))))
    } catch {
      case e: TypecheckException =>
        val msg = e.getMessage
        abort(s"Unexpected error during type check.\nMessage: $msg\nType: $tree\nRaw: ${showRaw(tree)}" )
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Three operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOpGen[F, N](implicit ev0: c.WeakTypeTag[F], evn: c.WeakTypeTag[N]): MaterializeOpAuxGen =
    new MaterializeOpAuxGen(weakTypeOf[F], weakTypeOf[N])

  def opCalc[T1, T2, T3](funcName : String, a : Calc, b : Calc, c : Calc)(implicit annotatedSym : TypeSymbol) : Calc = {
    def unsupported() = abort(s"Unsupported $funcName[$a, $b, $c]")

    funcName match {
      case "Id" => a match {
        case t : CalcLit => t
        case t : CalcNLit => t
        case _ => unsupported()
      }
      case "Negate" => a match {
        case CalcLit.Char(t) => CalcLit(-t)
        case CalcLit.Int(t) => CalcLit(-t)
        case CalcLit.Long(t) => CalcLit(-t)
        case CalcLit.Float(t) => CalcLit(-t)
        case CalcLit.Double(t) => CalcLit(-t)
        case nl : CalcNLit => CalcNLit(q"-$nl")
        case _ => unsupported()
      }
      case "ToNat" => a match { //Has a special case to handle this in MaterializeOpAuxGen
        case CalcLit.Char(t) => CalcLit(t.toInt)
        case CalcLit.Int(t) => CalcLit(t.toInt)
        case CalcLit.Long(t) => CalcLit(t.toInt)
        case CalcLit.Float(t) => CalcLit(t.toInt)
        case CalcLit.Double(t) => CalcLit(t.toInt)
        case nl : CalcNLit => CalcNLit(q"$nl.toInt")
        case _ => unsupported()
      }
      case "ToChar" => a match {
        case CalcLit.Char(t) => CalcLit(t.toChar)
        case CalcLit.Int(t) => CalcLit(t.toChar)
        case CalcLit.Long(t) => CalcLit(t.toChar)
        case CalcLit.Float(t) => CalcLit(t.toChar)
        case CalcLit.Double(t) => CalcLit(t.toChar)
        case nl : CalcNLit => CalcNLit(q"$nl.toChar")
        case _ => unsupported()
      }
      case "ToInt" => a match {
        case CalcLit.Char(t) => CalcLit(t.toInt)
        case CalcLit.Int(t) => CalcLit(t.toInt)
        case CalcLit.Long(t) => CalcLit(t.toInt)
        case CalcLit.Float(t) => CalcLit(t.toInt)
        case CalcLit.Double(t) => CalcLit(t.toInt)
        case nl : CalcNLit => CalcNLit(q"$nl.toInt")
        case _ => unsupported()
      }
      case "ToLong" => a match {
        case CalcLit.Char(t) => CalcLit(t.toLong)
        case CalcLit.Int(t) => CalcLit(t.toLong)
        case CalcLit.Long(t) => CalcLit(t.toLong)
        case CalcLit.Float(t) => CalcLit(t.toLong)
        case CalcLit.Double(t) => CalcLit(t.toLong)
        case nl : CalcNLit => CalcNLit(q"$nl.toLong")
        case _ => unsupported()
      }
      case "ToFloat" => a match {
        case CalcLit.Char(t) => CalcLit(t.toFloat)
        case CalcLit.Int(t) => CalcLit(t.toFloat)
        case CalcLit.Long(t) => CalcLit(t.toFloat)
        case CalcLit.Float(t) => CalcLit(t.toFloat)
        case CalcLit.Double(t) => CalcLit(t.toFloat)
        case nl : CalcNLit => CalcNLit(q"$nl.toFloat")
        case _ => unsupported()
      }
      case "ToDouble" => a match {
        case CalcLit.Char(t) => CalcLit(t.toDouble)
        case CalcLit.Int(t) => CalcLit(t.toDouble)
        case CalcLit.Long(t) => CalcLit(t.toDouble)
        case CalcLit.Float(t) => CalcLit(t.toDouble)
        case CalcLit.Double(t) => CalcLit(t.toDouble)
        case nl : CalcNLit => CalcNLit(q"$nl.toDouble")
        case _ => unsupported()
      }
      case "ToString" => a match {
        case CalcLit.Char(t) => CalcLit(t.toString)
        case CalcLit.Int(t) => CalcLit(t.toString)
        case CalcLit.Long(t) => CalcLit(t.toString)
        case CalcLit.Float(t) => CalcLit(t.toString)
        case CalcLit.Double(t) => CalcLit(t.toString)
        case CalcLit.String(t) => CalcLit(t.toString)
        case CalcLit.Boolean(t) => CalcLit(t.toString)
        case nl : CalcNLit => CalcNLit(q"$nl.toString")
        case _ => unsupported()
      }

//      case "IsNat" => q"$aTree.isInstanceOf[Int] && $aTree >= 0"
//      case "IsChar" => q"$aTree.isInstanceOf[Char]"
//      case "IsInt" => q"$aTree.isInstanceOf[Int]"
//      case "IsLong" => q"$aTree.isInstanceOf[Long]"
//      case "IsFloat" => q"$aTree.isInstanceOf[Float]"
//      case "IsDouble" => q"$aTree.isInstanceOf[Double]"
//      case "IsString" => q"$aTree.isInstanceOf[String]"
//      case "IsBoolean" => q"$aTree.isInstanceOf[Boolean]"
//      case "Negate" => q"-$aTree"
//      case "Abs" => a match {
//        case CalcLitTree(at) =>
//          evalTyped(aTree) match {
//            case (Constant(t : Int)) =>
//            case (Constant(t : Long)) =>
//            case (Constant(t : Float)) =>
//            case (Constant(t : Double)) =>
//          }
//        case CalcNLitTree(at) => aTree
//      }

//      case ("IsNat",      a: Char, _, _)              => CalcLit(false)
//      case ("IsNat",      a: Int, _, _)               => CalcLit(a >= 0)
//      case ("IsNat",      a: Long, _, _)              => CalcLit(false)
//      case ("IsNat",      a: Float, _, _)             => CalcLit(false)
//      case ("IsNat",      a: Double, _, _)            => CalcLit(false)
//      case ("IsNat",      a: String, _, _)            => CalcLit(false)
//      case ("IsNat",      a: Boolean, _, _)           => CalcLit(false)
//
//      case ("IsChar",     a: Char, _, _)              => CalcLit(true)
//      case ("IsChar",     a: Int, _, _)               => CalcLit(false)
//      case ("IsChar",     a: Long, _, _)              => CalcLit(false)
//      case ("IsChar",     a: Float, _, _)             => CalcLit(false)
//      case ("IsChar",     a: Double, _, _)            => CalcLit(false)
//      case ("IsChar",     a: String, _, _)            => CalcLit(false)
//      case ("IsChar",     a: Boolean, _, _)           => CalcLit(false)
//
//      case ("IsInt",      a: Char, _, _)              => CalcLit(false)
//      case ("IsInt",      a: Int, _, _)               => CalcLit(true)
//      case ("IsInt",      a: Long, _, _)              => CalcLit(false)
//      case ("IsInt",      a: Float, _, _)             => CalcLit(false)
//      case ("IsInt",      a: Double, _, _)            => CalcLit(false)
//      case ("IsInt",      a: String, _, _)            => CalcLit(false)
//      case ("IsInt",      a: Boolean, _, _)           => CalcLit(false)
//
//      case ("IsLong",     a: Char, _, _)              => CalcLit(false)
//      case ("IsLong",     a: Int, _, _)               => CalcLit(false)
//      case ("IsLong",     a: Long, _, _)              => CalcLit(true)
//      case ("IsLong",     a: Float, _, _)             => CalcLit(false)
//      case ("IsLong",     a: Double, _, _)            => CalcLit(false)
//      case ("IsLong",     a: String, _, _)            => CalcLit(false)
//      case ("IsLong",     a: Boolean, _, _)           => CalcLit(false)
//
//      case ("IsFloat",    a: Char, _, _)              => CalcLit(false)
//      case ("IsFloat",    a: Int, _, _)               => CalcLit(false)
//      case ("IsFloat",    a: Long, _, _)              => CalcLit(false)
//      case ("IsFloat",    a: Float, _, _)             => CalcLit(true)
//      case ("IsFloat",    a: Double, _, _)            => CalcLit(false)
//      case ("IsFloat",    a: String, _, _)            => CalcLit(false)
//      case ("IsFloat",    a: Boolean, _, _)           => CalcLit(false)
//
//      case ("IsDouble",   a: Char, _, _)              => CalcLit(false)
//      case ("IsDouble",   a: Int, _, _)               => CalcLit(false)
//      case ("IsDouble",   a: Long, _, _)              => CalcLit(false)
//      case ("IsDouble",   a: Float, _, _)             => CalcLit(false)
//      case ("IsDouble",   a: Double, _, _)            => CalcLit(true)
//      case ("IsDouble",   a: String, _, _)            => CalcLit(false)
//      case ("IsDouble",   a: Boolean, _, _)           => CalcLit(false)
//
//      case ("IsString",   a: Char, _, _)              => CalcLit(false)
//      case ("IsString",   a: Int, _, _)               => CalcLit(false)
//      case ("IsString",   a: Long, _, _)              => CalcLit(false)
//      case ("IsString",   a: Float, _, _)             => CalcLit(false)
//      case ("IsString",   a: Double, _, _)            => CalcLit(false)
//      case ("IsString",   a: String, _, _)            => CalcLit(true)
//      case ("IsString",   a: Boolean, _, _)           => CalcLit(false)
//
//      case ("IsBoolean",  a: Char, _, _)              => CalcLit(false)
//      case ("IsBoolean",  a: Int, _, _)               => CalcLit(false)
//      case ("IsBoolean",  a: Long, _, _)              => CalcLit(false)
//      case ("IsBoolean",  a: Float, _, _)             => CalcLit(false)
//      case ("IsBoolean",  a: Double, _, _)            => CalcLit(false)
//      case ("IsBoolean",  a: String, _, _)            => CalcLit(false)
//      case ("IsBoolean",  a: Boolean, _, _)           => CalcLit(true)
//
//      case ("Negate",     a: Char, _, _)              => CalcLit(-a)
//      case ("Negate",     a: Int, _, _)               => CalcLit(-a)
//      case ("Negate",     a: Long, _, _)              => CalcLit(-a)
//      case ("Negate",     a: Float, _, _)             => CalcLit(-a)
//      case ("Negate",     a: Double, _, _)            => CalcLit(-a)
//
//      case ("Abs",        a: Int, _, _)               => CalcLit(abs(a))
//      case ("Abs",        a: Long, _, _)              => CalcLit(abs(a))
//      case ("Abs",        a: Float, _, _)             => CalcLit(abs(a))
//      case ("Abs",        a: Double, _, _)            => CalcLit(abs(a))
//
//      case ("NumberOfLeadingZeros", a: Int, _, _)     => CalcLit(java.lang.Integer.numberOfLeadingZeros(a))
//      case ("NumberOfLeadingZeros", a: Long, _, _)    => CalcLit(java.lang.Long.numberOfLeadingZeros(a))
//
//      case ("Floor",      a: Float, _, _)             => CalcLit(floor(a.toDouble))
//      case ("Floor",      a: Double, _, _)            => CalcLit(floor(a))
//
//      case ("Ceil",       a: Float, _, _)             => CalcLit(ceil(a.toDouble))
//      case ("Ceil",       a: Double, _, _)            => CalcLit(ceil(a))
//
//      case ("Round",      a: Float, _, _)             => CalcLit(round(a))
//      case ("Round",      a: Double, _, _)            => CalcLit(round(a))
//
//      case ("Sin",        a: Float, _, _)             => CalcLit(sin(a.toDouble))
//      case ("Sin",        a: Double, _, _)            => CalcLit(sin(a))
//
//      case ("Cos",        a: Float, _, _)             => CalcLit(cos(a.toDouble))
//      case ("Cos",        a: Double, _, _)            => CalcLit(cos(a))
//
//      case ("Tan",        a: Float, _, _)             => CalcLit(tan(a.toDouble))
//      case ("Tan",        a: Double, _, _)            => CalcLit(tan(a))
//
//      case ("Sqrt",       a: Float, _, _)             => CalcLit(sqrt(a.toDouble))
//      case ("Sqrt",       a: Double, _, _)            => CalcLit(sqrt(a))
//
//      case ("Log",        a: Float, _, _)             => CalcLit(log(a.toDouble))
//      case ("Log",        a: Double, _, _)            => CalcLit(log(a))
//
//      case ("Log10",      a: Float, _, _)             => CalcLit(log10(a.toDouble))
//      case ("Log10",      a: Double, _, _)            => CalcLit(log10(a))
//
//      case ("Reverse",    a: String, _, _)            => CalcLit(a.reverse)
//      case ("!",          a: Boolean, _, _)           => CalcLit(!a)
//      case ("Require",    a: Boolean, b: String, _)   =>
//        if (!a)
//          abort(b)
//        else
//          CalcLit(a)
//      case ("==>",        _,          b,          _)  => CalcLit(b)
//
//      case ("+",          a: Char,    b: Char,    _)  => CalcLit(a + b)
//      case ("+",          a: Char,    b: Int,     _)  => CalcLit(a + b)
//      case ("+",          a: Char,    b: Long,    _)  => CalcLit(a + b)
//      case ("+",          a: Char,    b: Float,   _)  => CalcLit(a + b)
//      case ("+",          a: Char,    b: Double,  _)  => CalcLit(a + b)
//      case ("+",          a: Int,     b: Char,    _)  => CalcLit(a + b)
//      case ("+",          a: Int,     b: Int,     _)  => CalcLit(a + b)
//      case ("+",          a: Int,     b: Long,    _)  => CalcLit(a + b)
//      case ("+",          a: Int,     b: Float,   _)  => CalcLit(a + b)
//      case ("+",          a: Int,     b: Double,  _)  => CalcLit(a + b)
//      case ("+",          a: Long,    b: Char,    _)  => CalcLit(a + b)
//      case ("+",          a: Long,    b: Int,     _)  => CalcLit(a + b)
//      case ("+",          a: Long,    b: Long,    _)  => CalcLit(a + b)
//      case ("+",          a: Long,    b: Float,   _)  => CalcLit(a + b)
//      case ("+",          a: Long,    b: Double,  _)  => CalcLit(a + b)
//      case ("+",          a: Float,   b: Char,    _)  => CalcLit(a + b)
//      case ("+",          a: Float,   b: Int,     _)  => CalcLit(a + b)
//      case ("+",          a: Float,   b: Long,    _)  => CalcLit(a + b)
//      case ("+",          a: Float,   b: Float,   _)  => CalcLit(a + b)
//      case ("+",          a: Float,   b: Double,  _)  => CalcLit(a + b)
//      case ("+",          a: Double,  b: Char,    _)  => CalcLit(a + b)
//      case ("+",          a: Double,  b: Int,     _)  => CalcLit(a + b)
//      case ("+",          a: Double,  b: Long,    _)  => CalcLit(a + b)
//      case ("+",          a: Double,  b: Float,   _)  => CalcLit(a + b)
//      case ("+",          a: Double,  b: Double,  _)  => CalcLit(a + b)
//      case ("+",          a: String,  b: String,  _)  => CalcLit(a + b) //Concat
//      case ("+",          a: Tree,    b: Int,     _)  => CalcNLitTree(q"$a + $b") //Concat
//
//      case ("-",          a: Char,    b: Char,    _)  => CalcLit(a - b)
//      case ("-",          a: Char,    b: Int,     _)  => CalcLit(a - b)
//      case ("-",          a: Char,    b: Long,    _)  => CalcLit(a - b)
//      case ("-",          a: Char,    b: Float,   _)  => CalcLit(a - b)
//      case ("-",          a: Char,    b: Double,  _)  => CalcLit(a - b)
//      case ("-",          a: Int,     b: Char,    _)  => CalcLit(a - b)
//      case ("-",          a: Int,     b: Int,     _)  => CalcLit(a - b)
//      case ("-",          a: Int,     b: Long,    _)  => CalcLit(a - b)
//      case ("-",          a: Int,     b: Float,   _)  => CalcLit(a - b)
//      case ("-",          a: Int,     b: Double,  _)  => CalcLit(a - b)
//      case ("-",          a: Long,    b: Char,    _)  => CalcLit(a - b)
//      case ("-",          a: Long,    b: Int,     _)  => CalcLit(a - b)
//      case ("-",          a: Long,    b: Long,    _)  => CalcLit(a - b)
//      case ("-",          a: Long,    b: Float,   _)  => CalcLit(a - b)
//      case ("-",          a: Long,    b: Double,  _)  => CalcLit(a - b)
//      case ("-",          a: Float,   b: Char,    _)  => CalcLit(a - b)
//      case ("-",          a: Float,   b: Int,     _)  => CalcLit(a - b)
//      case ("-",          a: Float,   b: Long,    _)  => CalcLit(a - b)
//      case ("-",          a: Float,   b: Float,   _)  => CalcLit(a - b)
//      case ("-",          a: Float,   b: Double,  _)  => CalcLit(a - b)
//      case ("-",          a: Double,  b: Char,    _)  => CalcLit(a - b)
//      case ("-",          a: Double,  b: Int,     _)  => CalcLit(a - b)
//      case ("-",          a: Double,  b: Long,    _)  => CalcLit(a - b)
//      case ("-",          a: Double,  b: Float,   _)  => CalcLit(a - b)
//      case ("-",          a: Double,  b: Double,  _)  => CalcLit(a - b)
//
//      case ("*",          a: Char,    b: Char,    _)  => CalcLit(a * b)
//      case ("*",          a: Char,    b: Int,     _)  => CalcLit(a * b)
//      case ("*",          a: Char,    b: Long,    _)  => CalcLit(a * b)
//      case ("*",          a: Char,    b: Float,   _)  => CalcLit(a * b)
//      case ("*",          a: Char,    b: Double,  _)  => CalcLit(a * b)
//      case ("*",          a: Int,     b: Char,    _)  => CalcLit(a * b)
//      case ("*",          a: Int,     b: Int,     _)  => CalcLit(a * b)
//      case ("*",          a: Int,     b: Long,    _)  => CalcLit(a * b)
//      case ("*",          a: Int,     b: Float,   _)  => CalcLit(a * b)
//      case ("*",          a: Int,     b: Double,  _)  => CalcLit(a * b)
//      case ("*",          a: Long,    b: Char,    _)  => CalcLit(a * b)
//      case ("*",          a: Long,    b: Int,     _)  => CalcLit(a * b)
//      case ("*",          a: Long,    b: Long,    _)  => CalcLit(a * b)
//      case ("*",          a: Long,    b: Float,   _)  => CalcLit(a * b)
//      case ("*",          a: Long,    b: Double,  _)  => CalcLit(a * b)
//      case ("*",          a: Float,   b: Char,    _)  => CalcLit(a * b)
//      case ("*",          a: Float,   b: Int,     _)  => CalcLit(a * b)
//      case ("*",          a: Float,   b: Long,    _)  => CalcLit(a * b)
//      case ("*",          a: Float,   b: Float,   _)  => CalcLit(a * b)
//      case ("*",          a: Float,   b: Double,  _)  => CalcLit(a * b)
//      case ("*",          a: Double,  b: Char,    _)  => CalcLit(a * b)
//      case ("*",          a: Double,  b: Int,     _)  => CalcLit(a * b)
//      case ("*",          a: Double,  b: Long,    _)  => CalcLit(a * b)
//      case ("*",          a: Double,  b: Float,   _)  => CalcLit(a * b)
//      case ("*",          a: Double,  b: Double,  _)  => CalcLit(a * b)
//
//      case ("/",          a: Char,    b: Char,    _)  => CalcLit(a / b)
//      case ("/",          a: Char,    b: Int,     _)  => CalcLit(a / b)
//      case ("/",          a: Char,    b: Long,    _)  => CalcLit(a / b)
//      case ("/",          a: Char,    b: Float,   _)  => CalcLit(a / b)
//      case ("/",          a: Char,    b: Double,  _)  => CalcLit(a / b)
//      case ("/",          a: Int,     b: Char,    _)  => CalcLit(a / b)
//      case ("/",          a: Int,     b: Int,     _)  => CalcLit(a / b)
//      case ("/",          a: Int,     b: Long,    _)  => CalcLit(a / b)
//      case ("/",          a: Int,     b: Float,   _)  => CalcLit(a / b)
//      case ("/",          a: Int,     b: Double,  _)  => CalcLit(a / b)
//      case ("/",          a: Long,    b: Char,    _)  => CalcLit(a / b)
//      case ("/",          a: Long,    b: Int,     _)  => CalcLit(a / b)
//      case ("/",          a: Long,    b: Long,    _)  => CalcLit(a / b)
//      case ("/",          a: Long,    b: Float,   _)  => CalcLit(a / b)
//      case ("/",          a: Long,    b: Double,  _)  => CalcLit(a / b)
//      case ("/",          a: Float,   b: Char,    _)  => CalcLit(a / b)
//      case ("/",          a: Float,   b: Int,     _)  => CalcLit(a / b)
//      case ("/",          a: Float,   b: Long,    _)  => CalcLit(a / b)
//      case ("/",          a: Float,   b: Float,   _)  => CalcLit(a / b)
//      case ("/",          a: Float,   b: Double,  _)  => CalcLit(a / b)
//      case ("/",          a: Double,  b: Char,    _)  => CalcLit(a / b)
//      case ("/",          a: Double,  b: Int,     _)  => CalcLit(a / b)
//      case ("/",          a: Double,  b: Long,    _)  => CalcLit(a / b)
//      case ("/",          a: Double,  b: Float,   _)  => CalcLit(a / b)
//      case ("/",          a: Double,  b: Double,  _)  => CalcLit(a / b)
//
//      case ("%",          a: Char,    b: Char,    _)  => CalcLit(a % b)
//      case ("%",          a: Char,    b: Int,     _)  => CalcLit(a % b)
//      case ("%",          a: Char,    b: Long,    _)  => CalcLit(a % b)
//      case ("%",          a: Char,    b: Float,   _)  => CalcLit(a % b)
//      case ("%",          a: Char,    b: Double,  _)  => CalcLit(a % b)
//      case ("%",          a: Int,     b: Char,    _)  => CalcLit(a % b)
//      case ("%",          a: Int,     b: Int,     _)  => CalcLit(a % b)
//      case ("%",          a: Int,     b: Long,    _)  => CalcLit(a % b)
//      case ("%",          a: Int,     b: Float,   _)  => CalcLit(a % b)
//      case ("%",          a: Int,     b: Double,  _)  => CalcLit(a % b)
//      case ("%",          a: Long,    b: Char,    _)  => CalcLit(a % b)
//      case ("%",          a: Long,    b: Int,     _)  => CalcLit(a % b)
//      case ("%",          a: Long,    b: Long,    _)  => CalcLit(a % b)
//      case ("%",          a: Long,    b: Float,   _)  => CalcLit(a % b)
//      case ("%",          a: Long,    b: Double,  _)  => CalcLit(a % b)
//      case ("%",          a: Float,   b: Char,    _)  => CalcLit(a % b)
//      case ("%",          a: Float,   b: Int,     _)  => CalcLit(a % b)
//      case ("%",          a: Float,   b: Long,    _)  => CalcLit(a % b)
//      case ("%",          a: Float,   b: Float,   _)  => CalcLit(a % b)
//      case ("%",          a: Float,   b: Double,  _)  => CalcLit(a % b)
//      case ("%",          a: Double,  b: Char,    _)  => CalcLit(a % b)
//      case ("%",          a: Double,  b: Int,     _)  => CalcLit(a % b)
//      case ("%",          a: Double,  b: Long,    _)  => CalcLit(a % b)
//      case ("%",          a: Double,  b: Float,   _)  => CalcLit(a % b)
//      case ("%",          a: Double,  b: Double,  _)  => CalcLit(a % b)
//
//      case ("Pow",        a: Float,   b: Float,   _)  => CalcLit(pow(a.toDouble,b.toDouble))
//      case ("Pow",        a: Float,   b: Double,  _)  => CalcLit(pow(a.toDouble,b.toDouble))
//      case ("Pow",        a: Double,  b: Float,   _)  => CalcLit(pow(a.toDouble,b.toDouble))
//      case ("Pow",        a: Double,  b: Double,  _)  => CalcLit(pow(a.toDouble,b.toDouble))
//
//      case ("==",         a: Char,    b: Char,    _)  => CalcLit(a == b)
//      case ("==",         a: Char,    b: Int,     _)  => CalcLit(a == b)
//      case ("==",         a: Char,    b: Long,    _)  => CalcLit(a == b)
//      case ("==",         a: Char,    b: Float,   _)  => CalcLit(a == b)
//      case ("==",         a: Char,    b: Double,  _)  => CalcLit(a == b)
//      case ("==",         a: Int,     b: Char,    _)  => CalcLit(a == b)
//      case ("==",         a: Int,     b: Int,     _)  => CalcLit(a == b)
//      case ("==",         a: Int,     b: Long,    _)  => CalcLit(a == b)
//      case ("==",         a: Int,     b: Float,   _)  => CalcLit(a == b)
//      case ("==",         a: Int,     b: Double,  _)  => CalcLit(a == b)
//      case ("==",         a: Long,    b: Char,    _)  => CalcLit(a == b)
//      case ("==",         a: Long,    b: Int,     _)  => CalcLit(a == b)
//      case ("==",         a: Long,    b: Long,    _)  => CalcLit(a == b)
//      case ("==",         a: Long,    b: Float,   _)  => CalcLit(a == b)
//      case ("==",         a: Long,    b: Double,  _)  => CalcLit(a == b)
//      case ("==",         a: Float,   b: Char,    _)  => CalcLit(a == b)
//      case ("==",         a: Float,   b: Int,     _)  => CalcLit(a == b)
//      case ("==",         a: Float,   b: Long,    _)  => CalcLit(a == b)
//      case ("==",         a: Float,   b: Float,   _)  => CalcLit(a == b)
//      case ("==",         a: Float,   b: Double,  _)  => CalcLit(a == b)
//      case ("==",         a: Double,  b: Char,    _)  => CalcLit(a == b)
//      case ("==",         a: Double,  b: Int,     _)  => CalcLit(a == b)
//      case ("==",         a: Double,  b: Long,    _)  => CalcLit(a == b)
//      case ("==",         a: Double,  b: Float,   _)  => CalcLit(a == b)
//      case ("==",         a: Double,  b: Double,  _)  => CalcLit(a == b)
//      case ("==",         a: String,  b: String,  _)  => CalcLit(a == b)
//      case ("==",         a: Boolean, b: Boolean, _)  => CalcLit(a == b)
//
//      case ("!=",         a: Char,    b: Char,    _)  => CalcLit(a != b)
//      case ("!=",         a: Char,    b: Int,     _)  => CalcLit(a != b)
//      case ("!=",         a: Char,    b: Long,    _)  => CalcLit(a != b)
//      case ("!=",         a: Char,    b: Float,   _)  => CalcLit(a != b)
//      case ("!=",         a: Char,    b: Double,  _)  => CalcLit(a != b)
//      case ("!=",         a: Int,     b: Char,    _)  => CalcLit(a != b)
//      case ("!=",         a: Int,     b: Int,     _)  => CalcLit(a != b)
//      case ("!=",         a: Int,     b: Long,    _)  => CalcLit(a != b)
//      case ("!=",         a: Int,     b: Float,   _)  => CalcLit(a != b)
//      case ("!=",         a: Int,     b: Double,  _)  => CalcLit(a != b)
//      case ("!=",         a: Long,    b: Char,    _)  => CalcLit(a != b)
//      case ("!=",         a: Long,    b: Int,     _)  => CalcLit(a != b)
//      case ("!=",         a: Long,    b: Long,    _)  => CalcLit(a != b)
//      case ("!=",         a: Long,    b: Float,   _)  => CalcLit(a != b)
//      case ("!=",         a: Long,    b: Double,  _)  => CalcLit(a != b)
//      case ("!=",         a: Float,   b: Char,    _)  => CalcLit(a != b)
//      case ("!=",         a: Float,   b: Int,     _)  => CalcLit(a != b)
//      case ("!=",         a: Float,   b: Long,    _)  => CalcLit(a != b)
//      case ("!=",         a: Float,   b: Float,   _)  => CalcLit(a != b)
//      case ("!=",         a: Float,   b: Double,  _)  => CalcLit(a != b)
//      case ("!=",         a: Double,  b: Char,    _)  => CalcLit(a != b)
//      case ("!=",         a: Double,  b: Int,     _)  => CalcLit(a != b)
//      case ("!=",         a: Double,  b: Long,    _)  => CalcLit(a != b)
//      case ("!=",         a: Double,  b: Float,   _)  => CalcLit(a != b)
//      case ("!=",         a: Double,  b: Double,  _)  => CalcLit(a != b)
//      case ("!=",         a: String,  b: String,  _)  => CalcLit(a != b)
//      case ("!=",         a: Boolean, b: Boolean, _)  => CalcLit(a != b)
//
//      case ("<",          a: Char,    b: Char,    _)  => CalcLit(a < b)
//      case ("<",          a: Char,    b: Int,     _)  => CalcLit(a < b)
//      case ("<",          a: Char,    b: Long,    _)  => CalcLit(a < b)
//      case ("<",          a: Char,    b: Float,   _)  => CalcLit(a < b)
//      case ("<",          a: Char,    b: Double,  _)  => CalcLit(a < b)
//      case ("<",          a: Int,     b: Char,    _)  => CalcLit(a < b)
//      case ("<",          a: Int,     b: Int,     _)  => CalcLit(a < b)
//      case ("<",          a: Int,     b: Long,    _)  => CalcLit(a < b)
//      case ("<",          a: Int,     b: Float,   _)  => CalcLit(a < b)
//      case ("<",          a: Int,     b: Double,  _)  => CalcLit(a < b)
//      case ("<",          a: Long,    b: Char,    _)  => CalcLit(a < b)
//      case ("<",          a: Long,    b: Int,     _)  => CalcLit(a < b)
//      case ("<",          a: Long,    b: Long,    _)  => CalcLit(a < b)
//      case ("<",          a: Long,    b: Float,   _)  => CalcLit(a < b)
//      case ("<",          a: Long,    b: Double,  _)  => CalcLit(a < b)
//      case ("<",          a: Float,   b: Char,    _)  => CalcLit(a < b)
//      case ("<",          a: Float,   b: Int,     _)  => CalcLit(a < b)
//      case ("<",          a: Float,   b: Long,    _)  => CalcLit(a < b)
//      case ("<",          a: Float,   b: Float,   _)  => CalcLit(a < b)
//      case ("<",          a: Float,   b: Double,  _)  => CalcLit(a < b)
//      case ("<",          a: Double,  b: Char,    _)  => CalcLit(a < b)
//      case ("<",          a: Double,  b: Int,     _)  => CalcLit(a < b)
//      case ("<",          a: Double,  b: Long,    _)  => CalcLit(a < b)
//      case ("<",          a: Double,  b: Float,   _)  => CalcLit(a < b)
//      case ("<",          a: Double,  b: Double,  _)  => CalcLit(a < b)
//
//      case (">",          a: Char,    b: Char,    _)  => CalcLit(a > b)
//      case (">",          a: Char,    b: Int,     _)  => CalcLit(a > b)
//      case (">",          a: Char,    b: Long,    _)  => CalcLit(a > b)
//      case (">",          a: Char,    b: Float,   _)  => CalcLit(a > b)
//      case (">",          a: Char,    b: Double,  _)  => CalcLit(a > b)
//      case (">",          a: Int,     b: Char,    _)  => CalcLit(a > b)
//      case (">",          a: Int,     b: Int,     _)  => CalcLit(a > b)
//      case (">",          a: Int,     b: Long,    _)  => CalcLit(a > b)
//      case (">",          a: Int,     b: Float,   _)  => CalcLit(a > b)
//      case (">",          a: Int,     b: Double,  _)  => CalcLit(a > b)
//      case (">",          a: Long,    b: Char,    _)  => CalcLit(a > b)
//      case (">",          a: Long,    b: Int,     _)  => CalcLit(a > b)
//      case (">",          a: Long,    b: Long,    _)  => CalcLit(a > b)
//      case (">",          a: Long,    b: Float,   _)  => CalcLit(a > b)
//      case (">",          a: Long,    b: Double,  _)  => CalcLit(a > b)
//      case (">",          a: Float,   b: Char,    _)  => CalcLit(a > b)
//      case (">",          a: Float,   b: Int,     _)  => CalcLit(a > b)
//      case (">",          a: Float,   b: Long,    _)  => CalcLit(a > b)
//      case (">",          a: Float,   b: Float,   _)  => CalcLit(a > b)
//      case (">",          a: Float,   b: Double,  _)  => CalcLit(a > b)
//      case (">",          a: Double,  b: Char,    _)  => CalcLit(a > b)
//      case (">",          a: Double,  b: Int,     _)  => CalcLit(a > b)
//      case (">",          a: Double,  b: Long,    _)  => CalcLit(a > b)
//      case (">",          a: Double,  b: Float,   _)  => CalcLit(a > b)
//      case (">",          a: Double,  b: Double,  _)  => CalcLit(a > b)
//
//      case ("<=",         a: Char,    b: Char,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Char,    b: Int,     _)  => CalcLit(a <= b)
//      case ("<=",         a: Char,    b: Long,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Char,    b: Float,   _)  => CalcLit(a <= b)
//      case ("<=",         a: Char,    b: Double,  _)  => CalcLit(a <= b)
//      case ("<=",         a: Int,     b: Char,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Int,     b: Int,     _)  => CalcLit(a <= b)
//      case ("<=",         a: Int,     b: Long,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Int,     b: Float,   _)  => CalcLit(a <= b)
//      case ("<=",         a: Int,     b: Double,  _)  => CalcLit(a <= b)
//      case ("<=",         a: Long,    b: Char,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Long,    b: Int,     _)  => CalcLit(a <= b)
//      case ("<=",         a: Long,    b: Long,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Long,    b: Float,   _)  => CalcLit(a <= b)
//      case ("<=",         a: Long,    b: Double,  _)  => CalcLit(a <= b)
//      case ("<=",         a: Float,   b: Char,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Float,   b: Int,     _)  => CalcLit(a <= b)
//      case ("<=",         a: Float,   b: Long,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Float,   b: Float,   _)  => CalcLit(a <= b)
//      case ("<=",         a: Float,   b: Double,  _)  => CalcLit(a <= b)
//      case ("<=",         a: Double,  b: Char,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Double,  b: Int,     _)  => CalcLit(a <= b)
//      case ("<=",         a: Double,  b: Long,    _)  => CalcLit(a <= b)
//      case ("<=",         a: Double,  b: Float,   _)  => CalcLit(a <= b)
//      case ("<=",         a: Double,  b: Double,  _)  => CalcLit(a <= b)
//
//      case (">=",         a: Char,    b: Char,    _)  => CalcLit(a >= b)
//      case (">=",         a: Char,    b: Int,     _)  => CalcLit(a >= b)
//      case (">=",         a: Char,    b: Long,    _)  => CalcLit(a >= b)
//      case (">=",         a: Char,    b: Float,   _)  => CalcLit(a >= b)
//      case (">=",         a: Char,    b: Double,  _)  => CalcLit(a >= b)
//      case (">=",         a: Int,     b: Char,    _)  => CalcLit(a >= b)
//      case (">=",         a: Int,     b: Int,     _)  => CalcLit(a >= b)
//      case (">=",         a: Int,     b: Long,    _)  => CalcLit(a >= b)
//      case (">=",         a: Int,     b: Float,   _)  => CalcLit(a >= b)
//      case (">=",         a: Int,     b: Double,  _)  => CalcLit(a >= b)
//      case (">=",         a: Long,    b: Char,    _)  => CalcLit(a >= b)
//      case (">=",         a: Long,    b: Int,     _)  => CalcLit(a >= b)
//      case (">=",         a: Long,    b: Long,    _)  => CalcLit(a >= b)
//      case (">=",         a: Long,    b: Float,   _)  => CalcLit(a >= b)
//      case (">=",         a: Long,    b: Double,  _)  => CalcLit(a >= b)
//      case (">=",         a: Float,   b: Char,    _)  => CalcLit(a >= b)
//      case (">=",         a: Float,   b: Int,     _)  => CalcLit(a >= b)
//      case (">=",         a: Float,   b: Long,    _)  => CalcLit(a >= b)
//      case (">=",         a: Float,   b: Float,   _)  => CalcLit(a >= b)
//      case (">=",         a: Float,   b: Double,  _)  => CalcLit(a >= b)
//      case (">=",         a: Double,  b: Char,    _)  => CalcLit(a >= b)
//      case (">=",         a: Double,  b: Int,     _)  => CalcLit(a >= b)
//      case (">=",         a: Double,  b: Long,    _)  => CalcLit(a >= b)
//      case (">=",         a: Double,  b: Float,   _)  => CalcLit(a >= b)
//      case (">=",         a: Double,  b: Double,  _)  => CalcLit(a >= b)
//
//      case ("&&",         a: Boolean, b: Boolean, _)  => CalcLit(a && b)
//      case ("||",         a: Boolean, b: Boolean, _)  => CalcLit(a || b)
//
//      case ("Min",        a: Int,     b: Int,     _)  => CalcLit(min(a, b))
//      case ("Min",        a: Long,    b: Long,    _)  => CalcLit(min(a, b))
//      case ("Min",        a: Float,   b: Float,   _)  => CalcLit(min(a, b))
//      case ("Min",        a: Double,  b: Double,  _)  => CalcLit(min(a, b))
//
//      case ("Max",        a: Int,     b: Int,     _)  => CalcLit(max(a, b))
//      case ("Max",        a: Long,    b: Long,    _)  => CalcLit(max(a, b))
//      case ("Max",        a: Float,   b: Float,   _)  => CalcLit(max(a, b))
//      case ("Max",        a: Double,  b: Double,  _)  => CalcLit(max(a, b))
//
//      case ("Substring",  a: String,  b: Int,     _)  => CalcLit(a.substring(b))
//      case ("Length",     a: String,  _,          _)  => CalcLit(a.length)
//      case ("CharAt",     a: String,  b: Int,     _)  => CalcLit(a.charAt(b))

      case _ => abort(s"Unsupported $funcName[$a, $b, $c]")
    }
  }

  final class MaterializeOpAuxGen(opTpe: Type, nTpe: Type) {
    def usingFuncName : Tree = {
      implicit val annotatedSym : TypeSymbol = symbolOf[OpMacro[_,_,_,_]]
      val funcName = extractSingletonValue(nTpe)
      val opResult = extractSingletonValue(opTpe)

      val genTree = (funcName, opResult) match {
        case (CalcLit("ToNat"), CalcLit.Int(t)) =>
          genOpTreeNat(opTpe, t)
        case (_, CalcLit(t)) =>
          genOpTreeLit(opTpe, t)
        case (_, CalcNLit(t)) =>
          genOpTreeWitness(opTpe, t)
//        case (CalcLit("AcceptNonLiteral"), CalcNLit(t)) => genOpTreeNLit(opTpe, t)
//        case (CalcLit("AcceptNonLiteral"), CalcUnknown(t)) => genOpTreeUnknown(opTpe, t)
        case _ => extractionFailed(opTpe)
      }

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
