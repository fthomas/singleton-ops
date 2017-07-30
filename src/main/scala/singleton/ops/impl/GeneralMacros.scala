package singleton.ops.impl
import macrocompat.bundle
import singleton.twoface.impl.TwoFaceAny

import scala.reflect.macros.whitebox
import scala.reflect.macros.TypecheckException
@bundle
trait GeneralMacros {
  val c: whitebox.Context

  import c.universe._


  object funcTypes {
    val Arg = symbolOf[OpId.Arg]
    val AcceptNonLiteral = symbolOf[OpId.AcceptNonLiteral]
    val Id = symbolOf[OpId.Id]
    val ToNat = symbolOf[OpId.ToNat]
    val ToChar = symbolOf[OpId.ToChar]
    val ToInt = symbolOf[OpId.ToInt]
    val ToLong = symbolOf[OpId.ToLong]
    val ToFloat = symbolOf[OpId.ToFloat]
    val ToDouble = symbolOf[OpId.ToDouble]
    val ToString = symbolOf[OpId.ToString]
    val IsNat = symbolOf[OpId.IsNat]
    val IsChar = symbolOf[OpId.IsChar]
    val IsInt = symbolOf[OpId.IsInt]
    val IsLong = symbolOf[OpId.IsLong]
    val IsFloat = symbolOf[OpId.IsFloat]
    val IsDouble = symbolOf[OpId.IsDouble]
    val IsString = symbolOf[OpId.IsString]
    val IsBoolean = symbolOf[OpId.IsBoolean]
    val Negate = symbolOf[OpId.Negate]
    val Abs = symbolOf[OpId.Abs]
    val NumberOfLeadingZeros = symbolOf[OpId.NumberOfLeadingZeros]
    val Floor = symbolOf[OpId.Floor]
    val Ceil = symbolOf[OpId.Ceil]
    val Round = symbolOf[OpId.Round]
    val Sin = symbolOf[OpId.Sin]
    val Cos = symbolOf[OpId.Cos]
    val Tan = symbolOf[OpId.Tan]
    val Sqrt = symbolOf[OpId.Sqrt]
    val Log = symbolOf[OpId.Log]
    val Log10 = symbolOf[OpId.Log10]
    val Reverse = symbolOf[OpId.Reverse]
    val ! = symbolOf[OpId.!]
    val Require = symbolOf[OpId.Require]
    val ITE = symbolOf[OpId.ITE]
    val IsNonLiteral = symbolOf[OpId.IsNonLiteral]
    val ==> = symbolOf[OpId.==>]
    val + = symbolOf[OpId.+]
    val - = symbolOf[OpId.-]
    val * = symbolOf[OpId.*]
    val / = symbolOf[OpId./]
    val % = symbolOf[OpId.%]
    val < = symbolOf[OpId.<]
    val > = symbolOf[OpId.>]
    val <= = symbolOf[OpId.<=]
    val >= = symbolOf[OpId.>=]
    val == = symbolOf[OpId.==]
    val != = symbolOf[OpId.!=]
    val && = symbolOf[OpId.&&]
    val || = symbolOf[OpId.||]
    val Pow = symbolOf[OpId.Pow]
    val Min = symbolOf[OpId.Min]
    val Max = symbolOf[OpId.Max]
    val Substring = symbolOf[OpId.Substring]
    val CharAt = symbolOf[OpId.CharAt]
    val Length = symbolOf[OpId.Length]
  }

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
    val tpe : Type
  }

  object Calc {
    sealed trait Char extends Calc{type T = scala.Char; val tpe = typeOf[scala.Char]}
    sealed trait Int extends Calc{type T = scala.Int; val tpe = typeOf[scala.Int]}
    sealed trait Long extends Calc{type T = scala.Long; val tpe = typeOf[scala.Long]}
    sealed trait Float extends Calc{type T = scala.Float; val tpe = typeOf[scala.Float]}
    sealed trait Double extends Calc{type T = scala.Double; val tpe = typeOf[scala.Double]}
    sealed trait String extends Calc{type T = java.lang.String; val tpe = typeOf[java.lang.String]}
    sealed trait Boolean extends Calc{type T = scala.Boolean; val tpe = typeOf[scala.Boolean]}
  }

  sealed trait CalcVal extends Calc {
    val value : T
    val tree : Tree
  }
  object CalcVal {
    sealed trait Kind
    object Lit extends Kind
    object NLit extends Kind
    implicit val lift = Liftable[CalcVal] {p => p.tree}
    class Char(val value : scala.Char, val tree : Tree) extends CalcVal with Calc.Char
    object Char {
      def apply(value : scala.Char, tree : Tree) : Char = new Char(value, tree)
      def unapply(arg: Char) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Int(val value : scala.Int, val tree : Tree) extends CalcVal with Calc.Int
    object Int {
      def apply(value : scala.Int, tree : Tree) : Int = new Int(value, tree)
      def unapply(arg: Int) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Long(val value : scala.Long, val tree : Tree) extends CalcVal with Calc.Long
    object Long {
      def apply(value : scala.Long, tree : Tree) : Long = new Long(value, tree)
      def unapply(arg: Long) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Float(val value : scala.Float, val tree : Tree) extends CalcVal with Calc.Float
    object Float {
      def apply(value : scala.Float, tree : Tree) : Float = new Float(value, tree)
      def unapply(arg: Float) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Double(val value : scala.Double, val tree : Tree) extends CalcVal with Calc.Double
    object Double {
      def apply(value : scala.Double, tree : Tree) : Double = new Double(value, tree)
      def unapply(arg: Double) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class String(val value : java.lang.String, val tree : Tree) extends CalcVal with Calc.String
    object String {
      def apply(value : java.lang.String, tree : Tree) : String = new String(value, tree)
      def unapply(arg: String) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Boolean(val value : scala.Boolean, val tree : Tree) extends CalcVal with Calc.Boolean
    object Boolean {
      def apply(value : scala.Boolean, tree : Tree) : Boolean = new Boolean(value, tree)
      def unapply(arg: Boolean) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    def apply[T](value : T, tree : Tree)(implicit unsupported : TypeSymbol, kind : Kind) = kind match {
      case Lit => CalcLit(value)
      case NLit => CalcNLit(value, tree)
    }
//    def unapply(arg: CalcVal) : Option[arg.T] = Some(arg.value)
  }

  sealed trait CalcLit extends CalcVal {
    override val tpe = constantTypeOf(value)
  }

  object CalcLit {
    implicit val lift = Liftable[CalcLit] { p => p.tree }
    case class Char(override val value : scala.Char) extends CalcVal.Char(value, Literal(Constant(value))) with CalcLit
    case class Int(override val value : scala.Int) extends CalcVal.Int(value, Literal(Constant(value))) with CalcLit
    case class Long(override val value : scala.Long) extends CalcVal.Long(value, Literal(Constant(value))) with CalcLit
    case class Float(override val value : scala.Float) extends CalcVal.Float(value, Literal(Constant(value))) with CalcLit
    case class Double(override val value : scala.Double) extends CalcVal.Double(value, Literal(Constant(value))) with CalcLit
    case class String(override val value : java.lang.String) extends CalcVal.String(value, Literal(Constant(value))) with CalcLit
    case class Boolean(override val value : scala.Boolean) extends CalcVal.Boolean(value, Literal(Constant(value))) with CalcLit
    def apply[T](t : T)(implicit unsupported : TypeSymbol) = t match {
      case t : scala.Char => Char(t)
      case t : scala.Int => Int(t)
      case t : scala.Long => Long(t)
      case t : scala.Float => Float(t)
      case t : scala.Double => Double(t)
      case t : java.lang.String => String(t)
      case t : scala.Boolean => Boolean(t)
      case _ => abort(s"Unsupported literal type: $t")
    }
    def unapply(arg: CalcLit) : Option[arg.T] = Some(arg.value)
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

  sealed trait CalcTFType extends Calc
  object CalcTFType {
    object Char extends CalcTFType with Calc.Char
    object Int extends CalcTFType with Calc.Int
    object Long extends CalcTFType with Calc.Long
    object Float extends CalcTFType with Calc.Float
    object Double extends CalcTFType with Calc.Double
    object String extends CalcTFType with Calc.String
    object Boolean extends CalcTFType with Calc.Boolean
  }

  sealed trait CalcNLit extends CalcVal
  object CalcNLit {
    implicit val lift = Liftable[CalcNLit] { p => p.tree }
    case class Char(override val tree : Tree) extends CalcVal.Char('\u0001', tree) with CalcNLit
    case class Int(override val tree : Tree) extends CalcVal.Int(1, tree) with CalcNLit
    case class Long(override val tree : Tree) extends CalcVal.Long(1L, tree) with CalcNLit
    case class Float(override val tree : Tree) extends CalcVal.Float(1.0f, tree) with CalcNLit
    case class Double(override val tree : Tree) extends CalcVal.Double(1.0, tree) with CalcNLit
    case class String(override val tree : Tree) extends CalcVal.String("1", tree) with CalcNLit
    case class Boolean(override val tree : Tree) extends CalcVal.Boolean(true, tree) with CalcNLit

//    def apply[T](valueRef : T, tree : Tree)(implicit unsupported : TypeSymbol) : CalcNLit = {
//      valueRef match {
//        case t : scala.Char => Char(tree)
//        case t : scala.Int => Int(tree)
//        case t : scala.Long => Long(tree)
//        case t : scala.Float => Float(tree)
//        case t : scala.Double => Double(tree)
//        case t : java.lang.String => String(tree)
//        case t : scala.Boolean => Boolean(tree)
//        case _ => abort("Unsupported type")
//      }
//    }
    def apply[T](valueRef : T, tree : Tree)(implicit unsupported : TypeSymbol) : CalcNLit =
      CalcNLit(CalcLit(valueRef), tree)
    def apply(calcTypeRef : Calc, tree : Tree)(implicit unsupported : TypeSymbol) : CalcNLit = {
      if (calcTypeRef.isInstanceOf[Calc.Char]) Char(tree)
      else if (calcTypeRef.isInstanceOf[Calc.Int]) Int(tree)
      else if (calcTypeRef.isInstanceOf[Calc.Long]) Long(tree)
      else if (calcTypeRef.isInstanceOf[Calc.Float]) Float(tree)
      else if (calcTypeRef.isInstanceOf[Calc.Double]) Double(tree)
      else if (calcTypeRef.isInstanceOf[Calc.String]) String(tree)
      else if (calcTypeRef.isInstanceOf[Calc.Boolean]) Boolean(tree)
      else abort("Unsupported type")
    }
    def unapply(arg: CalcNLit) : Option[Tree] = Some(arg.tree)
  }
  case class CalcUnknown(t: Type) extends Calc {
    val tpe = t
  }
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
          CalcLit.Int(calcNat(args.head).value + 1)
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
        case Some(t : CalcTFType) =>
          Some(CalcNLit(t, q"valueOf[$tp].getValue"))
        case Some(t : CalcType) =>
          Some(CalcNLit(t, q"valueOf[$tp]"))
        case _ =>
          Some(CalcUnknown(tp))
      }
    }

    def unapplyOpTwoFace(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      val tfTypeArg = tp.typeArgs.head
      unapplyOpArg(tfTypeArg) match {
        case Some(t : CalcLit) => Some(t)
        case Some(CalcType.Char) => Some(CalcTFType.Char)
        case Some(CalcType.Int) => Some(CalcTFType.Int)
        case Some(CalcType.Long) => Some(CalcTFType.Long)
        case Some(CalcType.Float) => Some(CalcTFType.Float)
        case Some(CalcType.Double) => Some(CalcTFType.Double)
        case Some(CalcType.String) => Some(CalcTFType.String)
        case Some(CalcType.Boolean) => Some(CalcTFType.Boolean)
        case _ => abort("Unsupported TwoFace argument: " + tfTypeArg)
      }
    }

    def unapplyOp(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      val args = tp.typeArgs
      val funcType = args.head.typeSymbol.asType

      //If function is set/get variable we keep the original string,
      //otherwise we get the variable's value
      val aValue = unapplyOpArg(args(1))
      val retVal = (funcType, aValue) match {
        case (funcTypes.IsNonLiteral, _) => //Looking for non literals
          aValue match {
            case Some(t : CalcLit) => Some(CalcLit(false))
            case _ => Some(CalcLit(true)) //non-literal type (e.g., Int, Long,...)
          }
        case (funcTypes.ITE, Some(CalcLit.Boolean(cond))) => //Special control case: ITE (If-Then-Else)
          if (cond)
            unapplyOpArg(args(2)) //true (then) part of the IF
          else
            unapplyOpArg(args(3)) //false (else) part of the IF
        case (funcTypes.ITE, Some(CalcNLit(cond))) => //Non-literal condition will return non-literal type
          val thenArg = unapplyOpArg(args(2))
          val elseArg = unapplyOpArg(args(3))
          (thenArg, elseArg) match {
            case (Some(thenArg0 : CalcVal), Some(elseArg0 : CalcVal)) => Some(CalcNLit(thenArg, q"if ($cond) $thenArg0 else $elseArg0"))
            case _ => None
          }
        case (funcTypes.Arg, Some(CalcLit.Int(argNum))) =>
          unapplyOpArg(args(2)) match { //Checking the argument type
            case (Some(t : CalcLit)) => Some(t) //Literal argument is just a literal
            case (Some(t)) => //Got a type, so returning argument name
              val term = TermName(s"arg$argNum")
              Some(CalcNLit(t, q"$term"))
            case _ =>
              None
          }

        case _ => //regular cases
          val bValue = unapplyOpArg(args(2))
          val cValue = unapplyOpArg(args(3))
          (aValue, bValue, cValue) match {
            case (Some(a : CalcVal), Some(b: CalcVal), Some(c : Calc)) =>
              Some(opCalc(funcType, a, b, c))
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
        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.Char[_]] => unapplyOpTwoFace(tp)
        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.Int[_]] => unapplyOpTwoFace(tp)
        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.Long[_]] => unapplyOpTwoFace(tp)
        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.Float[_]] => unapplyOpTwoFace(tp)
        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.Double[_]] => unapplyOpTwoFace(tp)
        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.String[_]] => unapplyOpTwoFace(tp)
        case TypeRef(_, sym, args) if sym == symbolOf[TwoFaceAny.Boolean[_]] => unapplyOpTwoFace(tp)
        ////////////////////////////////////////////////////////////////////////


        case TypeRef(_, sym, _) if sym.isAliasType =>
          val tpDealias = tp.dealias
          if (tpDealias == tp)
            abort("Unable to dealias type: " + showRaw(tp))
          else
            unapply(tpDealias)
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
    q"""
      new $opTpe {
        type OutWide = $outWideTpe
        type Out = $outTpe
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
        type $outTypeName = $outTpe
        final val value: $outTpe = $outTree
        final val isLiteral = true
        final val valueWide: $outWideTpe = $outWideLiteral
      }
      """
  }

  def genOpTreeNLit(opTpe : Type, calc : CalcNLit)(implicit annotatedSym : TypeSymbol) : Tree = {
    val valueTree = calc.tree
    val outTpe = runtimeTypeOf(calc.value)
    q"""
      new $opTpe {
        type OutWide = $outTpe
        type Out = $outTpe
        final val value: $outTpe = $valueTree
        final val isLiteral = false
        final val valueWide: $outTpe = $valueTree
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

  def extractValueFromTwoFace[T](tfTree : c.Tree)(implicit annotatedSym : TypeSymbol) : CalcVal = {
    val typedTree = c.typecheck(tfTree)
    extractSingletonValue(typedTree.tpe) match {
      case t : CalcLit => t
      case t : CalcTFType => CalcNLit(t, q"$tfTree.getValue")
      case t => CalcNLit(t, tfTree)
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
  def materializeOpGen[F](implicit ev0: c.WeakTypeTag[F]): MaterializeOpAuxGen =
    new MaterializeOpAuxGen(weakTypeOf[F])

  def opCalc(funcType : TypeSymbol, a : CalcVal, b : CalcVal, c : Calc)(implicit annotatedSym : TypeSymbol) : CalcVal = {
    def unsupported() = abort(s"Unsupported $funcType[$a, $b, $c]")

    //The output val is literal if all arguments are literal. Otherwise, it is non-literal.
    implicit val cvKind : CalcVal.Kind = (a, b, c) match {
      case (_ : CalcLit, _ : CalcLit, _ : CalcLit) => CalcVal.Lit
      case _ => CalcVal.NLit
    }

    def AcceptNonLiteral = a
    def Id = a
    def ToNat = a match { //Has a special case to handle this in MaterializeOpAuxGen
      case CalcVal.Char(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Int(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Long(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Float(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Double(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case _ => unsupported()
    }
    def ToChar = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal.Int(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal.Long(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal.Float(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal.Double(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case _ => unsupported()
    }
    def ToInt = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Int(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Long(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Float(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Double(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.String(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case _ => unsupported()
    }
    def ToLong = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.Int(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.Long(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.Float(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.Double(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.String(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case _ => unsupported()
    }
    def ToFloat = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.Int(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.Long(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.Float(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.Double(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.String(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case _ => unsupported()
    }
    def ToDouble = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Int(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Long(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Float(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Double(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.String(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case _ => unsupported()
    }
    def ToString = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Int(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Long(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Float(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Double(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.String(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Boolean(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case _ => unsupported()
    }
    def IsNat = a match {
      case CalcLit.Int(t) => CalcLit(t >= 0)
      case CalcNLit.Int(tt) => CalcNLit.Boolean(q"$tt >= 0")
      case _ => CalcLit(false)
    }
    def IsChar = a match {
      case t : Calc.Char => CalcLit(true)
      case _ => CalcLit(false)
    }
    def IsInt = a match {
      case t : Calc.Int => CalcLit(true)
      case _ => CalcLit(false)
    }
    def IsLong = a match {
      case t : Calc.Long => CalcLit(true)
      case _ => CalcLit(false)
    }
    def IsFloat = a match {
      case t : Calc.Float => CalcLit(true)
      case _ => CalcLit(false)
    }
    def IsDouble = a match {
      case t : Calc.Double => CalcLit(true)
      case _ => CalcLit(false)
    }
    def IsString = a match {
      case t : Calc.String => CalcLit(true)
      case _ => CalcLit(false)
    }
    def IsBoolean = a match {
      case t : Calc.Boolean => CalcLit(true)
      case _ => CalcLit(false)
    }
    def Negate = a match {
      case CalcVal.Char(t, tt) => CalcVal(-t, q"-$tt")
      case CalcVal.Int(t, tt) => CalcVal(-t, q"-$tt")
      case CalcVal.Long(t, tt) => CalcVal(-t, q"-$tt")
      case CalcVal.Float(t, tt) => CalcVal(-t, q"-$tt")
      case CalcVal.Double(t, tt) => CalcVal(-t, q"-$tt")
      case _ => unsupported()
    }
    def Abs = a match {
      case CalcVal.Int(t, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case CalcVal.Long(t, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case CalcVal.Float(t, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case CalcVal.Double(t, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case _ => unsupported()
    }
    def NumberOfLeadingZeros = a match {
      case CalcVal.Int(t, tt) => CalcVal(nlz(t), q"_root_.singleton.ops.impl.nlz($tt)")
      case CalcVal.Long(t, tt) => CalcVal(nlz(t), q"_root_.singleton.ops.impl.nlz($tt)")
      case _ => unsupported()
    }
    def Floor = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.floor(t.toDouble), q"_root_.scala.math.floor($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.floor(t), q"_root_.scala.math.floor($tt)")
      case _ => unsupported()
    }
    def Ceil = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.ceil(t.toDouble), q"_root_.scala.math.ceil($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.ceil(t), q"_root_.scala.math.ceil($tt)")
      case _ => unsupported()
    }
    def Round = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.round(t.toDouble), q"_root_.scala.math.round($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.round(t), q"_root_.scala.math.round($tt)")
      case _ => unsupported()
    }
    def Sin = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.sin(t.toDouble), q"_root_.scala.math.sin($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.sin(t), q"_root_.scala.math.sin($tt)")
      case _ => unsupported()
    }
    def Cos = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.cos(t.toDouble), q"_root_.scala.math.cos($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.cos(t), q"_root_.scala.math.cos($tt)")
      case _ => unsupported()
    }
    def Tan = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.tan(t.toDouble), q"_root_.scala.math.tan($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.tan(t), q"_root_.scala.math.tan($tt)")
      case _ => unsupported()
    }
    def Sqrt = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.sqrt(t.toDouble), q"_root_.scala.math.sqrt($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.sqrt(t), q"_root_.scala.math.sqrt($tt)")
      case _ => unsupported()
    }
    def Log = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.log(t.toDouble), q"_root_.scala.math.log($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.log(t), q"_root_.scala.math.log($tt)")
      case _ => unsupported()
    }
    def Log10 = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.log10(t.toDouble), q"_root_.scala.math.log10($tt.toDouble)")
      case CalcVal.Double(t, tt) => CalcVal(math.log10(t), q"_root_.scala.math.log10($tt)")
      case _ => unsupported()
    }
    def Reverse = a match {
      case CalcVal.String(t, tt) => CalcVal(t.reverse, q"$tt.reverse")
      case _ => unsupported()
    }
    def Not = a match {
      case CalcVal.Boolean(t, tt) => CalcVal(!t, q"!$tt")
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
        case msg : CalcNLit => CalcNLit.Boolean(q"require(false, $msg); false")
        case _ => unsupported()
      }
      case cond : CalcNLit => b match {
        case msg : CalcVal => CalcNLit.Boolean(q"require($cond, $msg); true")
        case _ => unsupported()
      }
      case _ => unsupported()
    }
    def ITE = (a, b, c) match {
      //Also has special case handling inside unapply
      case (CalcLit.Boolean(cond), CalcLit.Char(thenVal), CalcLit.Char(elseVal)) => CalcLit(if(cond) thenVal else elseVal)
      case (CalcLit.Boolean(cond), CalcLit.Int(thenVal), CalcLit.Int(elseVal)) => CalcLit(if(cond) thenVal else elseVal)
      case (CalcLit.Boolean(cond), CalcLit.Long(thenVal), CalcLit.Long(elseVal)) => CalcLit(if(cond) thenVal else elseVal)
      case (CalcLit.Boolean(cond), CalcLit.Float(thenVal), CalcLit.Float(elseVal)) => CalcLit(if(cond) thenVal else elseVal)
      case (CalcLit.Boolean(cond), CalcLit.Double(thenVal), CalcLit.Double(elseVal)) => CalcLit(if(cond) thenVal else elseVal)
      case (CalcLit.Boolean(cond), CalcLit.String(thenVal), CalcLit.String(elseVal)) => CalcLit(if(cond) thenVal else elseVal)
      case (CalcLit.Boolean(cond), CalcLit.Boolean(thenVal), CalcLit.Boolean(elseVal)) => CalcLit(if(cond) thenVal else elseVal)
      case (av : CalcLit, bv : CalcLit, cv : CalcLit) => unsupported()
      case (av : CalcVal, bv : CalcVal, cv : CalcVal) => CalcNLit(bv, q"if ($av) $bv else $cv")
      case _ => unsupported()
    }
    def Next = b match {
      case bv : CalcVal => bv
      case _ => unsupported()
    }
    def Plus = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.String(at, att), CalcVal.String(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case _ => unsupported()
    }
    def Minus = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case _ => unsupported()
    }
    def Mul = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case _ => unsupported()
    }
    def Div = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case _ => unsupported()
    }
    def Mod = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case _ => unsupported()
    }
    def Sml = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case _ => unsupported()
    }
    def Big = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case _ => unsupported()
    }
    def SmlEq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case _ => unsupported()
    }
    def BigEq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case _ => unsupported()
    }
    def Eq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.String(at, att), CalcVal.String(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case _ => unsupported()
    }
    def Neq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Char(at, att), CalcVal.Int(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Char(at, att), CalcVal.Long(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Char(at, att), CalcVal.Float(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Char(at, att), CalcVal.Double(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Int(at, att), CalcVal.Char(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Int(at, att), CalcVal.Long(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Int(at, att), CalcVal.Float(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Int(at, att), CalcVal.Double(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Long(at, att), CalcVal.Char(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Long(at, att), CalcVal.Int(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Long(at, att), CalcVal.Float(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Long(at, att), CalcVal.Double(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Float(at, att), CalcVal.Char(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Float(at, att), CalcVal.Int(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Float(at, att), CalcVal.Long(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Double(at, att), CalcVal.Char(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Double(at, att), CalcVal.Int(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Double(at, att), CalcVal.Long(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.String(at, att), CalcVal.String(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case _ => unsupported()
    }
    def And = (a, b) match {
      case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at && bt, q"$att && $btt")
      case _ => unsupported()
    }
    def Or = (a, b) match {
      case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at || bt, q"$att || $btt")
      case _ => unsupported()
    }
    def Pow = (a, b) match {
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) =>
        CalcVal(math.pow(at.toDouble, bt.toDouble), q"_root_.scala.math.pow($att.toDouble, $btt.toDouble)")
      case (CalcVal.Float(at, att), CalcVal.Double(bt, btt)) =>
        CalcVal(math.pow(at.toDouble, bt.toDouble), q"_root_.scala.math.pow($att.toDouble, $btt.toDouble)")
      case (CalcVal.Double(at, att), CalcVal.Float(bt, btt)) =>
        CalcVal(math.pow(at.toDouble, bt.toDouble), q"_root_.scala.math.pow($att.toDouble, $btt.toDouble)")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) =>
        CalcVal(math.pow(at.toDouble, bt.toDouble), q"_root_.scala.math.pow($att.toDouble, $btt.toDouble)")
      case _ => unsupported()
    }
    def Min = (a, b) match {
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case _ => unsupported()
    }
    def Max = (a, b) match {
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case _ => unsupported()
    }
    def Substring = (a, b) match {
      case (CalcVal.String(at, att), CalcVal.Int(bt, btt)) => CalcVal(at.substring(bt), q"$att.substring($btt)")
      case _ => unsupported()
    }
    def CharAt = (a, b) match {
      case (CalcVal.String(at, att), CalcVal.Int(bt, btt)) => CalcVal(at.charAt(bt), q"$att.charAt($btt)")
      case _ => unsupported()
    }
    def Length = a match {
      case CalcVal.String(at, att) => CalcVal(at.length, q"$att.length")
      case _ => unsupported()
    }

    funcType match {
      case funcTypes.AcceptNonLiteral => AcceptNonLiteral
      case funcTypes.Id => Id
      case funcTypes.ToNat => ToNat
      case funcTypes.ToChar => ToChar
      case funcTypes.ToInt => ToInt
      case funcTypes.ToLong => ToLong
      case funcTypes.ToFloat => ToFloat
      case funcTypes.ToDouble => ToDouble
      case funcTypes.ToString => ToString
      case funcTypes.IsNat => IsNat
      case funcTypes.IsChar => IsChar
      case funcTypes.IsInt => IsInt
      case funcTypes.IsLong => IsLong
      case funcTypes.IsFloat => IsFloat
      case funcTypes.IsDouble => IsDouble
      case funcTypes.IsString => IsString
      case funcTypes.IsBoolean => IsBoolean
      case funcTypes.Negate => Negate
      case funcTypes.Abs => Abs
      case funcTypes.NumberOfLeadingZeros => NumberOfLeadingZeros
      case funcTypes.Floor => Floor
      case funcTypes.Ceil => Ceil
      case funcTypes.Round => Round
      case funcTypes.Sin => Sin
      case funcTypes.Cos => Cos
      case funcTypes.Tan => Tan
      case funcTypes.Sqrt => Sqrt
      case funcTypes.Log => Log
      case funcTypes.Log10 => Log10
      case funcTypes.Reverse => Reverse
      case funcTypes.! => Not
      case funcTypes.Require => Require
      case funcTypes.ITE => ITE
      case funcTypes.==> => Next
      case funcTypes.+ => Plus
      case funcTypes.- => Minus
      case funcTypes.* => Mul
      case funcTypes./ => Div
      case funcTypes.% => Mod
      case funcTypes.< => Sml
      case funcTypes.> => Big
      case funcTypes.<= => SmlEq
      case funcTypes.>= => BigEq
      case funcTypes.== => Eq
      case funcTypes.!= => Neq
      case funcTypes.&& => And
      case funcTypes.|| => Or
      case funcTypes.Pow => Pow
      case funcTypes.Min => Min
      case funcTypes.Max => Max
      case funcTypes.Substring => Substring
      case funcTypes.CharAt => CharAt
      case funcTypes.Length => Length
      case _ => abort(s"Unsupported $funcType[$a, $b, $c]")
    }
  }

  final class MaterializeOpAuxGen(opTpe: Type) {
    def usingFuncName : Tree = {
      implicit val annotatedSym : TypeSymbol = symbolOf[OpMacro[_,_,_,_]]
      val funcType = opTpe.typeArgs.head.typeSymbol.asType
      val opResult = extractSingletonValue(opTpe)

      val genTree = (funcType, opResult) match {
        case (funcTypes.ToNat, CalcLit.Int(t)) => genOpTreeNat(opTpe, t)
        case (_, CalcLit(t)) => genOpTreeLit(opTpe, t)
        case (funcTypes.AcceptNonLiteral, t : CalcNLit) => genOpTreeNLit(opTpe, t)
        case (_, t: CalcNLit) =>
          abort("Calculation has returned a non-literal type/value.\nTo accept non-literal values, use `AcceptNonLiteral[T]`.")
        case _ => extractionFailed(opTpe)
      }

//      print(genTree)
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////
  // TwoFace
  ///////////////////////////////////////////////////////////////////////////////////////////
  def TwoFaceShellMaterializer[Shell](implicit shell : c.WeakTypeTag[Shell])
  : TwoFaceShellMaterializer[Shell] = new TwoFaceShellMaterializer[Shell](weakTypeOf[Shell])

  final class TwoFaceShellMaterializer[Shell](shellTpe : Type) {
    def shell1() : c.Tree = {
      implicit val annotatedSym : TypeSymbol = shellTpe.typeSymbol.asType
      val funcApplyTpe = shellTpe.typeArgs(0)
      val funcArgsTpe = shellTpe.typeArgs(1)
      val arg1Tpe = shellTpe.typeArgs(2)
      val arg1WideTpe = shellTpe.typeArgs(3)
      val outTpe = extractSingletonValue(funcApplyTpe).tpe
      val tfTerm = TermName(outTpe.widen.typeSymbol.name.toString)
      val tfType = TypeName(outTpe.widen.typeSymbol.name.toString)
      val genTree = extractSingletonValue(funcArgsTpe) match {
        case (t : CalcVal) =>
          q"""
             new $shellTpe {
               type Out = $outTpe
               type TF[T] = _root_.singleton.twoface.TwoFace.$tfType[T]
               def apply(arg1 : $arg1WideTpe) : _root_.singleton.twoface.TwoFace.$tfType[$outTpe] = {
                 _root_.singleton.twoface.TwoFace.$tfTerm($t)
               }
             }
           """
        case _ => extractionFailed(shellTpe)
      }
      //      print(showCode(genTree))
      genTree
    }
    def shell2() : c.Tree = {
      implicit val annotatedSym : TypeSymbol = shellTpe.typeSymbol.asType
      val funcApplyTpe = shellTpe.typeArgs(0)
      val funcArgsTpe = shellTpe.typeArgs(1)
      val arg1Tpe = shellTpe.typeArgs(2)
      val arg1WideTpe = shellTpe.typeArgs(3)
      val arg2Tpe = shellTpe.typeArgs(4)
      val arg2WideTpe = shellTpe.typeArgs(5)
      val outTpe = extractSingletonValue(funcApplyTpe).tpe
      val tfTerm = TermName(outTpe.widen.typeSymbol.name.toString)
      val tfType = TypeName(outTpe.widen.typeSymbol.name.toString)
      val genTree = extractSingletonValue(funcArgsTpe) match {
        case (t : CalcVal) =>
          q"""
             new $shellTpe {
               type Out = $outTpe
               type TF[T] = _root_.singleton.twoface.TwoFace.$tfType[T]
               def apply(arg1 : $arg1WideTpe, arg2 : $arg2WideTpe) : _root_.singleton.twoface.TwoFace.$tfType[$outTpe] = {
                 _root_.singleton.twoface.TwoFace.$tfTerm($t)
               }
             }
           """
        case _ => extractionFailed(shellTpe)
      }
//      print(showCode(genTree))
      genTree
    }
    def shell3() : c.Tree = {
      implicit val annotatedSym : TypeSymbol = shellTpe.typeSymbol.asType
      val funcApplyTpe = shellTpe.typeArgs(0)
      val funcArgsTpe = shellTpe.typeArgs(1)
      val arg1Tpe = shellTpe.typeArgs(2)
      val arg1WideTpe = shellTpe.typeArgs(3)
      val arg2Tpe = shellTpe.typeArgs(4)
      val arg2WideTpe = shellTpe.typeArgs(5)
      val arg3Tpe = shellTpe.typeArgs(6)
      val arg3WideTpe = shellTpe.typeArgs(7)
      val outTpe = extractSingletonValue(funcApplyTpe).tpe
      val tfTerm = TermName(outTpe.widen.typeSymbol.name.toString)
      val tfType = TypeName(outTpe.widen.typeSymbol.name.toString)
      val genTree = extractSingletonValue(funcArgsTpe) match {
        case (t : CalcVal) =>
          q"""
             new $shellTpe {
               type Out = $outTpe
               type TF[T] = _root_.singleton.twoface.TwoFace.$tfType[T]
               def apply(arg1 : $arg1WideTpe, arg2 : $arg2WideTpe, arg3 : $arg3WideTpe) : _root_.singleton.twoface.TwoFace.$tfType[$outTpe] = {
                 _root_.singleton.twoface.TwoFace.$tfTerm($t)
               }
             }
           """
        case _ => extractionFailed(shellTpe)
      }
      //      print(showCode(genTree))
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
