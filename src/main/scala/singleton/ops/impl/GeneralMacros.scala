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
    object TwoFace extends CalcType
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
        case TypeRef(pre, sym, Nil) =>
          calcNat(sym.info asSeenFrom (pre, sym.owner))
        case _ =>
          abort(s"Given Nat type is defective: $tp, raw: ${showRaw(tp)}")
      }
    }
    ////////////////////////////////////////////////////////////////////////

    def unapplyOpArg(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      unapply(tp) match {
        case Some(t : CalcLit) =>
          Some(t)
        case Some(t : CalcNLit) =>
          Some(t)
        case Some(CalcType.TwoFace) =>
          Some(CalcNLit(q"_root_.shapeless.Witness[$tp].value.getValue"))
        case Some(t : CalcType) =>
          Some(CalcNLit(q"_root_.shapeless.Witness[$tp].value"))
        case _ =>
          Some(CalcUnknown(tp))
      }
    }

    def unapplyOpTwoFace(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      unapplyOpArg(tp.typeArgs.head) match {
        case Some(t : CalcLit) => Some(t)
        case _ => Some(CalcType.TwoFace)
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

//            print(tp + " RAW " + showRaw(tp))
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
        case TypeRef(_, sym, args) if sym == symbolOf[OpMacro[_,_,_,_]] => unapplyOp(tp)
        case TypeRef(_, sym, args) if sym == symbolOf[OpGen[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpNat[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpChar[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpInt[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpLong[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpFloat[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpDouble[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpString[_]] => unapplyOpArg(args.head)
        case TypeRef(_, sym, args) if sym == symbolOf[OpBoolean[_]] => unapplyOpArg(args.head)
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // TwoFace Values
        ////////////////////////////////////////////////////////////////////////
//        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.Int[_]] => unapplyOpTwoFace(tp)
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
        type Out = $outTpe
        type Value = $outTpe
        type $outTypeName = $outTpe
        final val value: $outTpe = $outWideLiteral
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
        type OutWide = Int
        type Out = Int
        type Value = Int
        final val value: Value = $dummy
        final val isLiteral = false
        final val valueWide: Int = $dummy
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
    def Id = a match {
      case t : CalcLit => t
      case t : CalcNLit => t
      case _ => unsupported()
    }
    def ToNat = a match { //Has a special case to handle this in MaterializeOpAuxGen
      case CalcLit.Char(t) => CalcLit(t.toInt)
      case CalcLit.Int(t) => CalcLit(t.toInt)
      case CalcLit.Long(t) => CalcLit(t.toInt)
      case CalcLit.Float(t) => CalcLit(t.toInt)
      case CalcLit.Double(t) => CalcLit(t.toInt)
      case nl : CalcNLit => CalcNLit(q"$nl.toInt")
      case _ => unsupported()
    }
    def ToChar = a match {
      case CalcLit.Char(t) => CalcLit(t.toChar)
      case CalcLit.Int(t) => CalcLit(t.toChar)
      case CalcLit.Long(t) => CalcLit(t.toChar)
      case CalcLit.Float(t) => CalcLit(t.toChar)
      case CalcLit.Double(t) => CalcLit(t.toChar)
      case nl : CalcNLit => CalcNLit(q"$nl.toChar")
      case _ => unsupported()
    }
    def ToInt = a match {
      case CalcLit.Char(t) => CalcLit(t.toInt)
      case CalcLit.Int(t) => CalcLit(t.toInt)
      case CalcLit.Long(t) => CalcLit(t.toInt)
      case CalcLit.Float(t) => CalcLit(t.toInt)
      case CalcLit.Double(t) => CalcLit(t.toInt)
      case CalcLit.String(t) => CalcLit(t.toInt)
      case nl : CalcNLit => CalcNLit(q"$nl.toInt")
      case _ => unsupported()
    }
    def ToLong = a match {
      case CalcLit.Char(t) => CalcLit(t.toLong)
      case CalcLit.Int(t) => CalcLit(t.toLong)
      case CalcLit.Long(t) => CalcLit(t.toLong)
      case CalcLit.Float(t) => CalcLit(t.toLong)
      case CalcLit.Double(t) => CalcLit(t.toLong)
      case CalcLit.String(t) => CalcLit(t.toLong)
      case nl : CalcNLit => CalcNLit(q"$nl.toLong")
      case _ => unsupported()
    }
    def ToFloat = a match {
      case CalcLit.Char(t) => CalcLit(t.toFloat)
      case CalcLit.Int(t) => CalcLit(t.toFloat)
      case CalcLit.Long(t) => CalcLit(t.toFloat)
      case CalcLit.Float(t) => CalcLit(t.toFloat)
      case CalcLit.Double(t) => CalcLit(t.toFloat)
      case CalcLit.String(t) => CalcLit(t.toFloat)
      case nl : CalcNLit => CalcNLit(q"$nl.toFloat")
      case _ => unsupported()
    }
    def ToDouble = a match {
      case CalcLit.Char(t) => CalcLit(t.toDouble)
      case CalcLit.Int(t) => CalcLit(t.toDouble)
      case CalcLit.Long(t) => CalcLit(t.toDouble)
      case CalcLit.Float(t) => CalcLit(t.toDouble)
      case CalcLit.Double(t) => CalcLit(t.toDouble)
      case CalcLit.String(t) => CalcLit(t.toDouble)
      case nl : CalcNLit => CalcNLit(q"$nl.toDouble")
      case _ => unsupported()
    }
    def ToString = a match {
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
    def IsNat = a match {
      case CalcLit.Int(t) => CalcLit(t >= 0)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"if ($nl.isInstanceOf[Int]) $nl.asInstanceOf[Int] >= 0 else false")
      case _ => unsupported()
    }
    def IsChar = a match {
      case CalcLit.Char(t) => CalcLit(true)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"$nl.isInstanceOf[Char]")
      case _ => unsupported()
    }
    def IsInt = a match {
      case CalcLit.Int(t) => CalcLit(true)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"$nl.isInstanceOf[Int]")
      case _ => unsupported()
    }
    def IsLong = a match {
      case CalcLit.Long(t) => CalcLit(true)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"$nl.isInstanceOf[Long]")
      case _ => unsupported()
    }
    def IsFloat = a match {
      case CalcLit.Float(t) => CalcLit(true)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"$nl.isInstanceOf[Float]")
      case _ => unsupported()
    }
    def IsDouble = a match {
      case CalcLit.Double(t) => CalcLit(true)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"$nl.isInstanceOf[Double]")
      case _ => unsupported()
    }
    def IsString = a match {
      case CalcLit.String(t) => CalcLit(true)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"$nl.isInstanceOf[String]")
      case _ => unsupported()
    }
    def IsBoolean = a match {
      case CalcLit.Boolean(t) => CalcLit(true)
      case cl : CalcLit => CalcLit(false)
      case nl : CalcNLit => CalcNLit(q"$nl.isInstanceOf[Boolean]")
      case _ => unsupported()
    }
    def Negate = a match {
      case CalcLit.Char(t) => CalcLit(-t)
      case CalcLit.Int(t) => CalcLit(-t)
      case CalcLit.Long(t) => CalcLit(-t)
      case CalcLit.Float(t) => CalcLit(-t)
      case CalcLit.Double(t) => CalcLit(-t)
      case nl : CalcNLit => CalcNLit(q"-$nl")
      case _ => unsupported()
    }
    def Abs = a match {
      case CalcLit.Int(t) => CalcLit(math.abs(t))
      case CalcLit.Long(t) => CalcLit(math.abs(t))
      case CalcLit.Float(t) => CalcLit(math.abs(t))
      case CalcLit.Double(t) => CalcLit(math.abs(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.abs($nl)")
      case _ => unsupported()
    }
    def NumberOfLeadingZeros = a match {
      case CalcLit.Int(t) => CalcLit(nlz(t))
      case CalcLit.Long(t) => CalcLit(nlz(t))
      case nl : CalcNLit => CalcNLit(q"_root_.singleton.ops.impl.nlz($nl)")
      case _ => unsupported()
    }
    def Floor = a match {
      case CalcLit.Float(t) => CalcLit(math.floor(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.floor(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.floor($nl.toDouble)")
      case _ => unsupported()
    }
    def Ceil = a match {
      case CalcLit.Float(t) => CalcLit(math.ceil(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.ceil(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.ceil($nl.toDouble)")
      case _ => unsupported()
    }
    def Round = a match {
      case CalcLit.Float(t) => CalcLit(math.round(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.round(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.round($nl.toDouble)")
      case _ => unsupported()
    }
    def Sin = a match {
      case CalcLit.Float(t) => CalcLit(math.sin(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.sin(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.sin($nl.toDouble)")
      case _ => unsupported()
    }
    def Cos = a match {
      case CalcLit.Float(t) => CalcLit(math.cos(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.cos(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.cos($nl.toDouble)")
      case _ => unsupported()
    }
    def Tan = a match {
      case CalcLit.Float(t) => CalcLit(math.tan(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.tan(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.tan($nl.toDouble)")
      case _ => unsupported()
    }
    def Sqrt = a match {
      case CalcLit.Float(t) => CalcLit(math.sqrt(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.sqrt(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.sqrt($nl.toDouble)")
      case _ => unsupported()
    }
    def Log = a match {
      case CalcLit.Float(t) => CalcLit(math.log(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.log(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.log($nl.toDouble)")
      case _ => unsupported()
    }
    def Log10 = a match {
      case CalcLit.Float(t) => CalcLit(math.log10(t.toDouble))
      case CalcLit.Double(t) => CalcLit(math.log10(t))
      case nl : CalcNLit => CalcNLit(q"_root_.scala.math.log10($nl.toDouble)")
      case _ => unsupported()
    }
    def Reverse = a match {
      case CalcLit.String(t) => CalcLit(t.reverse)
      case nl : CalcNLit => CalcNLit(q"$nl.reverse")
      case _ => unsupported()
    }
    def Not = a match {
      case CalcLit.Boolean(t) => CalcLit(!t)
      case nl : CalcNLit => CalcNLit(q"!$nl")
      case _ => unsupported()
    }
    def Require = a match {
      case CalcLit.Boolean(true) => CalcLit(true)
      case CalcLit.Boolean(false) => b match {
        case CalcLit.String(msg) => c match {
          case CalcUnknown(t) => //redirection of implicit not found annotation is required to the given symbol
            implicit val annotatedSym : TypeSymbol = t.typeSymbol.asType
            abort(msg)
          case _ => abort(msg)
        }
        case msg : CalcNLit => CalcNLit(q"require(false, $msg); false")
        case _ => unsupported()
      }
      case cond : CalcNLit => b match {
        case msg : CalcVal => CalcNLit(q"require($cond, $msg); true")
        case _ => unsupported()
      }
      case _ => unsupported()
    }
    def ITE = (a, b, c) match {
      //Already handled literal ITE call as a special case
      case (av : CalcLit, bv : CalcLit, cv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal, cv : CalcVal) => CalcNLit(q"if ($av) $bv else $cv")
      case _ => unsupported()
    }
    def Next = b match {
      case bv : CalcVal => b
      case _ => unsupported()
    }
    def Plus = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at + bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at + bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at + bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at + bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at + bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at + bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at + bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at + bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at + bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at + bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at + bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at + bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at + bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at + bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at + bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at + bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at + bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at + bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at + bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at + bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at + bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at + bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at + bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at + bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at + bt)
      case (CalcLit.String(at), CalcLit.String(bt)) => CalcLit(at + bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av + $bv")
      case _ => unsupported()
    }
    def Minus = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at - bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at - bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at - bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at - bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at - bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at - bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at - bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at - bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at - bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at - bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at - bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at - bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at - bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at - bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at - bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at - bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at - bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at - bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at - bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at - bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at - bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at - bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at - bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at - bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at - bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av - $bv")
      case _ => unsupported()
    }
    def Mul = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at * bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at * bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at * bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at * bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at * bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at * bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at * bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at * bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at * bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at * bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at * bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at * bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at * bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at * bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at * bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at * bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at * bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at * bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at * bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at * bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at * bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at * bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at * bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at * bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at * bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av * $bv")
      case _ => unsupported()
    }
    def Div = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at / bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at / bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at / bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at / bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at / bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at / bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at / bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at / bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at / bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at / bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at / bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at / bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at / bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at / bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at / bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at / bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at / bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at / bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at / bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at / bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at / bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at / bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at / bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at / bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at / bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av / $bv")
      case _ => unsupported()
    }
    def Mod = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at % bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at % bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at % bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at % bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at % bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at % bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at % bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at % bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at % bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at % bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at % bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at % bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at % bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at % bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at % bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at % bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at % bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at % bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at % bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at % bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at % bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at % bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at % bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at % bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at % bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av % $bv")
      case _ => unsupported()
    }
    def Sml = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at < bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at < bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at < bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at < bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at < bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at < bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at < bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at < bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at < bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at < bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at < bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at < bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at < bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at < bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at < bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at < bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at < bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at < bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at < bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at < bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at < bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at < bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at < bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at < bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at < bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av < $bv")
      case _ => unsupported()
    }
    def Big = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at > bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at > bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at > bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at > bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at > bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at > bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at > bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at > bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at > bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at > bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at > bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at > bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at > bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at > bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at > bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at > bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at > bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at > bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at > bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at > bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at > bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at > bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at > bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at > bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at > bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av > $bv")
      case _ => unsupported()
    }
    def SmlEq = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at <= bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at <= bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at <= bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at <= bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at <= bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at <= bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at <= bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at <= bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at <= bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at <= bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at <= bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at <= bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at <= bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at <= bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at <= bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at <= bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at <= bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at <= bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at <= bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at <= bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at <= bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at <= bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at <= bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at <= bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at <= bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av <= $bv")
      case _ => unsupported()
    }
    def BigEq = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at >= bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at >= bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at >= bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at >= bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at >= bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at >= bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at >= bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at >= bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at >= bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at >= bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at >= bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at >= bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at >= bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at >= bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at >= bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at >= bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at >= bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at >= bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at >= bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at >= bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at >= bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at >= bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at >= bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at >= bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at >= bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av >= $bv")
      case _ => unsupported()
    }
    def Eq = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at == bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at == bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at == bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at == bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at == bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at == bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at == bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at == bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at == bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at == bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at == bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at == bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at == bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at == bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at == bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at == bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at == bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at == bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at == bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at == bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at == bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at == bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at == bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at == bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at == bt)
      case (CalcLit.Boolean(at), CalcLit.Boolean(bt)) => CalcLit(at == bt)
      case (CalcLit.String(at), CalcLit.String(bt)) => CalcLit(at == bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av == $bv")
      case _ => unsupported()
    }
    def Neq = (a, b) match {
      case (CalcLit.Char(at), CalcLit.Char(bt)) => CalcLit(at != bt)
      case (CalcLit.Char(at), CalcLit.Int(bt)) => CalcLit(at != bt)
      case (CalcLit.Char(at), CalcLit.Long(bt)) => CalcLit(at != bt)
      case (CalcLit.Char(at), CalcLit.Float(bt)) => CalcLit(at != bt)
      case (CalcLit.Char(at), CalcLit.Double(bt)) => CalcLit(at != bt)
      case (CalcLit.Int(at), CalcLit.Char(bt)) => CalcLit(at != bt)
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(at != bt)
      case (CalcLit.Int(at), CalcLit.Long(bt)) => CalcLit(at != bt)
      case (CalcLit.Int(at), CalcLit.Float(bt)) => CalcLit(at != bt)
      case (CalcLit.Int(at), CalcLit.Double(bt)) => CalcLit(at != bt)
      case (CalcLit.Long(at), CalcLit.Char(bt)) => CalcLit(at != bt)
      case (CalcLit.Long(at), CalcLit.Int(bt)) => CalcLit(at != bt)
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(at != bt)
      case (CalcLit.Long(at), CalcLit.Float(bt)) => CalcLit(at != bt)
      case (CalcLit.Long(at), CalcLit.Double(bt)) => CalcLit(at != bt)
      case (CalcLit.Float(at), CalcLit.Char(bt)) => CalcLit(at != bt)
      case (CalcLit.Float(at), CalcLit.Int(bt)) => CalcLit(at != bt)
      case (CalcLit.Float(at), CalcLit.Long(bt)) => CalcLit(at != bt)
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(at != bt)
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(at != bt)
      case (CalcLit.Double(at), CalcLit.Char(bt)) => CalcLit(at != bt)
      case (CalcLit.Double(at), CalcLit.Int(bt)) => CalcLit(at != bt)
      case (CalcLit.Double(at), CalcLit.Long(bt)) => CalcLit(at != bt)
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(at != bt)
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(at != bt)
      case (CalcLit.Boolean(at), CalcLit.Boolean(bt)) => CalcLit(at != bt)
      case (CalcLit.String(at), CalcLit.String(bt)) => CalcLit(at != bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av != $bv")
      case _ => unsupported()
    }
    def And = (a, b) match {
      case (CalcLit.Boolean(at), CalcLit.Boolean(bt)) => CalcLit(at && bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av && $bv")
      case _ => unsupported()
    }
    def Or = (a, b) match {
      case (CalcLit.Boolean(at), CalcLit.Boolean(bt)) => CalcLit(at || bt)
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av || $bv")
      case _ => unsupported()
    }
    def Pow = (a, b) match {
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(math.pow(at.toDouble, bt.toDouble))
      case (CalcLit.Float(at), CalcLit.Double(bt)) => CalcLit(math.pow(at.toDouble, bt.toDouble))
      case (CalcLit.Double(at), CalcLit.Float(bt)) => CalcLit(math.pow(at.toDouble, bt.toDouble))
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(math.pow(at.toDouble, bt.toDouble))
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"_root_.scala.math.pow($av.toDouble, $bv.toDouble)")
      case _ => unsupported()
    }
    def Min = (a, b) match {
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(math.min(at, bt))
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(math.min(at, bt))
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(math.min(at, bt))
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(math.min(at, bt))
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"_root_.scala.math.min($av, $bv)")
      case _ => unsupported()
    }
    def Max = (a, b) match {
      case (CalcLit.Int(at), CalcLit.Int(bt)) => CalcLit(math.max(at, bt))
      case (CalcLit.Long(at), CalcLit.Long(bt)) => CalcLit(math.max(at, bt))
      case (CalcLit.Float(at), CalcLit.Float(bt)) => CalcLit(math.max(at, bt))
      case (CalcLit.Double(at), CalcLit.Double(bt)) => CalcLit(math.max(at, bt))
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"_root_.scala.math.max($av, $bv)")
      case _ => unsupported()
    }
    def Substring = (a, b) match {
      case (CalcLit.String(at), CalcLit.Int(bt)) => CalcLit(at.substring(bt))
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av.substring($bv)")
      case _ => unsupported()
    }
    def CharAt = (a, b) match {
      case (CalcLit.String(at), CalcLit.Int(bt)) => CalcLit(at.charAt(bt))
      case (av : CalcLit, bv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal) => CalcNLit(q"$av.substring($bv)")
      case _ => unsupported()
    }
    def Length = a match {
      case CalcLit.String(at) => CalcLit(at.length)
      case av : CalcNLit => CalcNLit(q"$av.length")
      case _ => unsupported()
    }

    funcName match {
      case "Id" => Id
      case "ToNat" => ToNat
      case "ToChar" => ToChar
      case "ToInt" => ToInt
      case "ToLong" => ToLong
      case "ToFloat" => ToFloat
      case "ToDouble" => ToDouble
      case "ToString" => ToString
      case "IsNat" => IsNat
      case "IsChar" => IsChar
      case "IsInt" => IsInt
      case "IsLong" => IsLong
      case "IsFloat" => IsFloat
      case "IsDouble" => IsDouble
      case "IsString" => IsString
      case "IsBoolean" => IsBoolean
      case "Negate" => Negate
      case "Abs" => Abs
      case "NumberOfLeadingZeros" => NumberOfLeadingZeros
      case "Floor" => Floor
      case "Ceil" => Ceil
      case "Round" => Round
      case "Sin" => Sin
      case "Cos" => Cos
      case "Tan" => Tan
      case "Sqrt" => Sqrt
      case "Log" => Log
      case "Log10" => Log10
      case "Reverse" => Reverse
      case "!" => Not
      case "Require" => Require
      case "==>" => Next
      case "+" => Plus
      case "-" => Minus
      case "*" => Mul
      case "/" => Div
      case "%" => Mod
      case "<" => Sml
      case ">" => Big
      case "<=" => SmlEq
      case ">=" => BigEq
      case "==" => Eq
      case "!=" => Neq
      case "&&" => And
      case "||" => Or
      case "Pow" => Pow
      case "Min" => Min
      case "Max" => Max
      case "Substring" => Substring
      case "CharAt" => CharAt
      case "Length" => Length
      case _ => abort(s"Unsupported $funcName[$a, $b, $c]")
    }
  }

  final class MaterializeOpAuxGen(opTpe: Type, nTpe: Type) {
    def usingFuncName : Tree = {
      implicit val annotatedSym : TypeSymbol = symbolOf[OpMacro[_,_,_,_]]
      val funcName = extractSingletonValue(nTpe)
      val opResult = extractSingletonValue(opTpe)

      val genTree = (funcName, opResult) match {
        case (CalcLit("ToNat"), CalcLit.Int(t)) => genOpTreeNat(opTpe, t)
        case (_, CalcLit(t)) => genOpTreeLit(opTpe, t)
        case (_, CalcNLit(t)) => genOpTreeWitness(opTpe, t)
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
