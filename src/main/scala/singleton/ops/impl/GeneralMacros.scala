package singleton.ops.impl
import singleton.twoface.impl.TwoFaceAny

import scala.reflect.macros.{TypecheckException, whitebox}

private object MacroCache {
  import scala.collection.mutable
  val cache = mutable.Map.empty[Any, Any]
  def get(key : Any) : Option[Any] = cache.get(key)
  def add[V <: Any](key : Any, value : V) : V = {cache += (key -> value); value}
  var errorCache : String = ""
  def clearErrorCache() : Unit = errorCache = ""
  def setErrorCache(msg : String) : Unit = errorCache = msg
  def getErrorMessage : String = errorCache
}
trait GeneralMacros {
  val c: whitebox.Context

  import c.universe._
  val defaultAnnotatedSym : Option[TypeSymbol] =
    if (c.enclosingImplicits.isEmpty) None else c.enclosingImplicits.last.pt match {
      case TypeRef(_,sym,_) => Some(sym.asType)
      case x => Some(x.typeSymbol.asType)
    }

  private val func1Sym = symbolOf[Function1[_,_]]

  object funcTypes {
    val Arg = symbolOf[OpId.Arg]
    val AcceptNonLiteral = symbolOf[OpId.AcceptNonLiteral]
    val GetArg = symbolOf[OpId.GetArg]
    val ImplicitFound = symbolOf[OpId.ImplicitFound]
    val EnumCount = symbolOf[OpId.EnumCount]
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
    val GetType = symbolOf[OpId.GetType]
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
    val SubSequence = symbolOf[OpId.SubSequence]
    val StartsWith = symbolOf[OpId.StartsWith]
    val EndsWith = symbolOf[OpId.EndsWith]
    val Head = symbolOf[OpId.Head]
    val Tail = symbolOf[OpId.Tail]
    val CharAt = symbolOf[OpId.CharAt]
    val Length = symbolOf[OpId.Length]
    val Matches = symbolOf[OpId.Matches]
    val FirstMatch = symbolOf[OpId.FirstMatch]
    val PrefixMatch = symbolOf[OpId.PrefixMatch]
    val ReplaceFirstMatch = symbolOf[OpId.ReplaceFirstMatch]
    val ReplaceAllMatches = symbolOf[OpId.ReplaceAllMatches]
  }

  ////////////////////////////////////////////////////////////////////
  // Code thanks to Shapeless
  // https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/lazy.scala
  ////////////////////////////////////////////////////////////////////
  def setAnnotation(msg: String, annotatedSym : TypeSymbol): Unit = {
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
  sealed trait Calc extends Product with Serializable {
    val primitive : Primitive
    val tpe : Type
  }
  object Calc {
    implicit def getPrimitive(from : Calc) : Primitive = from.primitive
  }
  sealed trait Primitive extends Product with Serializable {
    val dummyConstant : Any
    val name : String
    val tpe : Type

    override def equals(that: Any): Boolean = {
      val thatPrim = that.asInstanceOf[Primitive]
      thatPrim.dummyConstant == dummyConstant && thatPrim.name == name && thatPrim.tpe =:= tpe
    }
  }
  object Primitive {
    case object Char    extends Primitive {val dummyConstant = '\u0001';val name = "Char";     val tpe = typeOf[scala.Char]}
    case object Int     extends Primitive {val dummyConstant = 1;       val name = "Int";      val tpe = typeOf[scala.Int]}
    case object Long    extends Primitive {val dummyConstant = 1L;      val name = "Long";     val tpe = typeOf[scala.Long]}
    case object Float   extends Primitive {val dummyConstant = 1.0f;    val name = "Float";    val tpe = typeOf[scala.Float]}
    case object Double  extends Primitive {val dummyConstant = 1.0;     val name = "Double";   val tpe = typeOf[scala.Double]}
    case object String  extends Primitive {val dummyConstant = "1";     val name = "String";   val tpe = typeOf[java.lang.String]}
    case object Boolean extends Primitive {val dummyConstant = true;    val name = "Boolean";  val tpe = typeOf[scala.Boolean]}
    case class Unknown(tpe : Type, name : String) extends Primitive {val dummyConstant: Any = None}
    def fromLiteral(lit : Any) : Primitive = lit match {
      case value : std.Char     => Primitive.Char
      case value : std.Int      => Primitive.Int
      case value : std.Long     => Primitive.Long
      case value : std.Float    => Primitive.Float
      case value : std.Double   => Primitive.Double
      case value : std.String   => Primitive.String
      case value : std.Boolean  => Primitive.Boolean
      case _ => abort(s"Unsupported literal type: $lit")
    }
  }
  sealed trait CalcVal extends Calc {
    val primitive : Primitive
    val literal : Option[Any]
    val tree : Tree
  }
  object CalcVal {
    sealed trait Kind
    object Lit extends Kind
    object NLit extends Kind
    implicit val lift = Liftable[CalcVal] {p => p.tree}
    def unapply(arg: CalcVal): Option[(Any, Tree)] = Some((arg.literal.getOrElse(arg.dummyConstant), arg.tree))
    def apply(value : Any, tree : Tree)(implicit kind : Kind) = kind match {
      case Lit => CalcLit(value)
      case NLit => CalcNLit(Primitive.fromLiteral(value), tree)
    }
    //use this when a literal calculation may fail
    def mayFail(primitive: Primitive, value : => Any, tree : Tree)(implicit kind : Kind) = kind match {
      case Lit => try{CalcLit(value)} catch {case e : Throwable => abort(e.getMessage)}
      case NLit => CalcNLit(primitive, tree)
    }
  }
  case class CalcLit(primitive : Primitive, value : Any) extends CalcVal {
    val literal = Some(value)
    val tpe : Type = constantTypeOf(value)
    val tree : Tree = Literal(Constant(value))
  }
  object CalcLit {
    object Char {
      def unapply(arg: CalcLit): Option[std.Char] = arg match {
        case CalcLit(Primitive.Char, value : std.Char) => Some(value)
        case _ => None
      }
    }
    object Int {
      def unapply(arg: CalcLit): Option[std.Int] = arg match {
        case CalcLit(Primitive.Int, value : std.Int) => Some(value)
        case _ => None
      }
    }
    object Long {
      def unapply(arg: CalcLit): Option[std.Long] = arg match {
        case CalcLit(Primitive.Long, value : std.Long) => Some(value)
        case _ => None
      }
    }
    object Float {
      def unapply(arg: CalcLit): Option[std.Float] = arg match {
        case CalcLit(Primitive.Float, value : std.Float) => Some(value)
        case _ => None
      }
    }
    object Double {
      def unapply(arg: CalcLit): Option[std.Double] = arg match {
        case CalcLit(Primitive.Double, value : std.Double) => Some(value)
        case _ => None
      }
    }
    object String {
      def unapply(arg: CalcLit): Option[std.String] = arg match {
        case CalcLit(Primitive.String, value : std.String) => Some(value)
        case _ => None
      }
    }
    object Boolean {
      def unapply(arg: CalcLit): Option[std.Boolean] = arg match {
        case CalcLit(Primitive.Boolean, value : std.Boolean) => Some(value)
        case _ => None
      }
    }
    def apply(t : Any) : CalcLit = CalcLit(Primitive.fromLiteral(t), t)
  }
  case class CalcNLit(primitive : Primitive, tree : Tree, tpe : Type) extends CalcVal {
    val literal = None
  }
  object CalcNLit {
    def apply(primitive: Primitive, tree: Tree): CalcNLit = new CalcNLit(primitive, tree, primitive.tpe)
  }

  sealed trait CalcType extends Calc
  object CalcType {
    case class Mark(primitive : Primitive) extends CalcType {
      val tpe = primitive.tpe
    }
    case class TF(primitive : Primitive) extends CalcType {
      val tpe = primitive.tpe
    }
    case class UB(primitive : Primitive) extends CalcType {
      val tpe = primitive.tpe
    }

    def unapply(arg: CalcType): Option[Primitive] = Some(arg.primitive)
  }
  case class CalcUnknown(tpe : Type, treeOption : Option[Tree], opIntercept : Boolean) extends Calc {
    override val primitive: Primitive = Primitive.Unknown(tpe, "Unknown")
  }
  object NonLiteralCalc {
    def unapply(tpe: Type): Option[CalcType.Mark] = tpe match {
      case TypeRef(_, sym, _) => sym match {
        case t if t == symbolOf[Char]             => Some(CalcType.Mark(Primitive.Char))
        case t if t == symbolOf[Int]              => Some(CalcType.Mark(Primitive.Int))
        case t if t == symbolOf[Long]             => Some(CalcType.Mark(Primitive.Long))
        case t if t == symbolOf[Float]            => Some(CalcType.Mark(Primitive.Float))
        case t if t == symbolOf[Double]           => Some(CalcType.Mark(Primitive.Double))
        case t if t == symbolOf[java.lang.String] => Some(CalcType.Mark(Primitive.String))
        case t if t == symbolOf[Boolean]          => Some(CalcType.Mark(Primitive.Boolean))
        case _ => None
      }
      case _ => None
    }
  }
  ////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////
  // Calc Caching
  ////////////////////////////////////////////////////////////////////
  object CalcCache {
    import collection.mutable
    import io.AnsiColor._
    def deepCopyTree(t: Tree): Tree = {
      val treeDuplicator = new Transformer {
        // by default Transformers don’t copy trees which haven’t been modified,
        // so we need to use use strictTreeCopier
        override val treeCopy =
        c.asInstanceOf[reflect.macros.runtime.Context].global.newStrictTreeCopier.asInstanceOf[TreeCopier]
      }

      treeDuplicator.transform(t)
    }

    final case class Key private (key : Type, argContext : List[Tree]) {
      override def equals(that: Any): Boolean = {
        val thatKey = that.asInstanceOf[Key]
        (thatKey.key =:= key) && (thatKey.argContext.length == argContext.length) &&
          ListZipper(thatKey.argContext, argContext).forall(_ equalsStructure _)
      }
    }
    object Key {
      implicit def fromType(key : Type) : Key = new Key(key, GetArgTree.argContext)
    }
    val cache = MacroCache.cache.asInstanceOf[mutable.Map[Key, Calc]]
    def get(key : Type) : Option[Calc] = {
      val k = Key.fromType(key)
      cache.get(k).map {v =>
        VerboseTraversal(s"${YELLOW}${BOLD}fetching${RESET} $k, $v")
        val cloned = v match {
          case lit : CalcLit => CalcLit(lit.value) //reconstruct internal literal tree
          case nlit : CalcNLit => CalcNLit(nlit.primitive, deepCopyTree(nlit.tree))
          case c => c
        }
        cloned
      }
    }
    def add[V <: Calc](key : Type, value : V) : V = {
      val k = Key.fromType(key)
      cache += (k -> value)
      VerboseTraversal(s"${GREEN}${BOLD}caching${RESET} $k -> $value")
      value
    }
  }
  ////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////
  // Code thanks to Paul Phillips
  // https://github.com/paulp/psply/blob/master/src/main/scala/PsplyMacros.scala
  ////////////////////////////////////////////////////////////////////
  import scala.reflect.internal.SymbolTable

  object VerboseTraversal {
    private val verboseTraversal = false
    private val indentSize = 2
    private var indent : Int = 0
    private def indentStr : String =  " " * (indentSize * indent)
    def incIdent : Unit = if (verboseTraversal) {
      indent = indent + 1
      println("--" * indent + ">")
    }
    def decIdent : Unit = if (verboseTraversal) {
      println("<" + "--" * indent)
      indent = indent - 1
    }
    def apply(s : String) : Unit = {
      if (verboseTraversal) println(indentStr + s.replaceAll("\n",s"\n$indentStr"))
    }
  }

  /** Typecheck singleton types so as to obtain indirectly
    *  available known-at-compile-time values.
    */
  object TypeCalc {
    ////////////////////////////////////////////////////////////////////////
    // Calculates the integer value of Shapeless Nat
    ////////////////////////////////////////////////////////////////////////
    object NatCalc {
      def unapply(tp: Type): Option[CalcLit] = {
        tp match {
          case TypeRef(_, sym, args) if sym == symbolOf[shapeless.Succ[_]] =>
            args.head match {
              case NatCalc(CalcLit.Int(value)) => Some(CalcLit(value + 1))
              case _ => abort(s"Given Nat type is defective: $tp, raw: ${showRaw(tp)}")
            }
          case TypeRef(_, sym, _) if sym == symbolOf[shapeless._0] =>
            Some(CalcLit(0))
          case TypeRef(pre, sym, Nil) =>
            unapply(sym.info asSeenFrom (pre, sym.owner))
          case _ =>
            None
        }
      }

    }
    ////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////
    // Calculates the TwoFace values
    ////////////////////////////////////////////////////////////////////////
    object TwoFaceCalc {
      def unapplyArg(calcTFType : Option[CalcType.TF], tfArgType : Type): Option[Calc] = {
        TypeCalc.unapply(tfArgType) match {
          case Some(t : CalcLit) => Some(t)
          case _ => calcTFType
        }
      }

      def unapply(tp: Type) : Option[Calc] = {
        val tfAnySym = symbolOf[TwoFaceAny[_,_]]
        tp match {
          case TypeRef(_, sym, args) if args.nonEmpty && tp.baseClasses.contains(tfAnySym) =>
            VerboseTraversal(s"@@TwoFaceCalc@@\nTP: $tp\nRAW: ${showRaw(tp)}\nBaseCls:${tp.baseClasses}")
            val calcTFType = sym match {
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Char[_]])     => Some(CalcType.TF(Primitive.Char))
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Int[_]])      => Some(CalcType.TF(Primitive.Int))
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Long[_]])     => Some(CalcType.TF(Primitive.Long))
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Float[_]])    => Some(CalcType.TF(Primitive.Float))
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Double[_]])   => Some(CalcType.TF(Primitive.Double))
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.String[_]])   => Some(CalcType.TF(Primitive.String))
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Boolean[_]])  => Some(CalcType.TF(Primitive.Boolean))
              case _ => None
            }
            if (calcTFType.isDefined)
              unapplyArg(calcTFType, tp.baseType(tfAnySym).typeArgs(1))
            else
              None
          case _ => None
        }
      }
    }
    ////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////
    // Calculates the different Op wrappers by unapplying their argument.
    ////////////////////////////////////////////////////////////////////////
    object OpCastCalc {
      def unapply(tp: Type): Option[Calc] = {
        tp match {
          case TypeRef(_, sym, args) =>
            sym match {
              case t if t == symbolOf[OpNat[_]] => Some(TypeCalc(args.head))
              case t if t == symbolOf[OpChar[_]] => Some(TypeCalc(args.head))
              case t if t == symbolOf[OpInt[_]] => Some(TypeCalc(args.head))
              case t if t == symbolOf[OpLong[_]] => Some(TypeCalc(args.head))
              case t if t == symbolOf[OpFloat[_]] => Some(TypeCalc(args.head))
              case t if t == symbolOf[OpDouble[_]] => Some(TypeCalc(args.head))
              case t if t == symbolOf[OpString[_]] => Some(TypeCalc(args.head))
              case t if t == symbolOf[OpBoolean[_]] => Some(TypeCalc(args.head))
              case _ => None
            }
          case _ =>
            None
        }
      }
    }
    ////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////
    // Calculates an Op
    ////////////////////////////////////////////////////////////////////////
    object OpCalc {
      private val opMacroSym = symbolOf[OpMacro[_,_,_,_]]
      private var uncachingReason : Int = 0
      def setUncachingReason(arg : Int) : Unit = {
        uncachingReason = arg
      }
      def unapply(tp: Type): Option[Calc] = {
        tp match {
          case TypeRef(_, sym, ft :: tp :: _) if sym == opMacroSym && ft.typeSymbol == funcTypes.GetType =>
            Some(CalcUnknown(tp, None, opIntercept = false))
          case TypeRef(_, sym, args) if sym == opMacroSym =>
            VerboseTraversal(s"@@OpCalc@@\nTP: $tp\nRAW: ${showRaw(tp)}")
            val funcType = args.head.typeSymbol.asType
            CalcCache.get(tp) match {
              case None =>
                val args = tp.typeArgs
                lazy val aValue = TypeCalc(args(1))
                lazy val bValue = TypeCalc(args(2))
                lazy val cValue = TypeCalc(args(3))

                //If function is set/get variable we keep the original string,
                //otherwise we get the variable's value
                val retVal = (funcType, aValue) match {
                  case (funcTypes.ImplicitFound, _) =>
                    setUncachingReason(1)
                    aValue match {
                      case CalcUnknown(t, _, false) => try {
                        c.typecheck(q"implicitly[$t]")
                        Some(CalcLit(true))
                      } catch {
                        case e : Throwable =>
                          Some(CalcLit(false))
                      }
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.EnumCount, _) =>
                    aValue match {
                      case CalcUnknown(t, _, false) => Some(CalcLit(t.typeSymbol.asClass.knownDirectSubclasses.size))
                      case _ => Some(CalcLit(0))
                    }
                  case (funcTypes.IsNat, _) =>
                    aValue match {
                      case CalcLit.Int(t) if t >= 0 => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsChar, _) =>
                    aValue.primitive match {
                      case Primitive.Char => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsInt, _) =>
                    aValue.primitive match {
                      case Primitive.Int => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsLong, _) =>
                    aValue.primitive match {
                      case Primitive.Long => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsFloat, _) =>
                    aValue.primitive match {
                      case Primitive.Float => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsDouble, _) =>
                    aValue.primitive match {
                      case Primitive.Double => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsString, _) =>
                    aValue.primitive match {
                      case Primitive.String => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsBoolean, _) =>
                    aValue.primitive match {
                      case Primitive.Boolean => Some(CalcLit(true))
                      case _ => Some(CalcLit(false))
                    }
                  case (funcTypes.IsNonLiteral, _) => //Looking for non literals
                    aValue match {
                      case t : CalcLit => Some(CalcLit(false))
                      case _ => Some(CalcLit(true)) //non-literal type (e.g., Int, Long,...)
                    }
                  case (funcTypes.ITE, CalcLit.Boolean(cond)) => //Special control case: ITE (If-Then-Else)
                    if (cond) Some(bValue) //true (then) part of the IF
                    else Some(cValue) //false (else) part of the IF
                  case (funcTypes.Arg, CalcLit.Int(argNum)) =>
                    bValue match { //Checking the argument type
                      case t : CalcLit => Some(t) //Literal argument is just a literal
                      case _ => //Got a type, so returning argument name
                        TypeCalc.unapply(args(3)) match {
                          case Some(t: CalcType) =>
                            val term = TermName(s"arg$argNum")
                            Some(CalcNLit(t, q"$term"))
                          case _ =>
                            None
                        }
                    }

                  case _ => //regular cases
                    opCalc(funcType, aValue, bValue, cValue) match {
                      case (res : CalcVal) => Some(res)
                      case u @ CalcUnknown(_,Some(_), _) => Some(u) //Accept unknown values with a tree
                      case oi @ CalcUnknown(_,_, true) => Some(oi) //Accept unknown op interception
                      case _ => None
                    }
                }
                if (uncachingReason > 0) VerboseTraversal(s"$uncachingReason:: Skipped caching of $tp")
                else retVal.foreach{rv => CalcCache.add(tp, rv)}
                retVal
              case cached => cached
            }
          case _ => None
        }
      }
    }
    ////////////////////////////////////////////////////////////////////////

    def apply(tp: Type): Calc = {
      TypeCalc.unapply(tp) match {
        case Some(t : CalcVal) => t
        case Some(t @ CalcType.UB(_)) => t
        case Some(t @ CalcType.TF(_)) => CalcNLit(t, q"valueOf[$tp].getValue")
        case Some(t : CalcType) => CalcNLit(t, q"valueOf[$tp]")
        case Some(t : CalcUnknown) => t
        case _ =>
          VerboseTraversal(s"@@Unknown@@\nTP: $tp\nRAW: ${showRaw(tp)}")
          CalcUnknown(tp, None, opIntercept = false)
      }
    }

    def unapply(tp: Type): Option[Calc] = {
      val g = c.universe.asInstanceOf[SymbolTable]
      implicit def fixSymbolOps(sym: Symbol): g.Symbol = sym.asInstanceOf[g.Symbol]

      VerboseTraversal(s"@@TypeCalc.unapply@@ ${c.enclosingPosition}\nTP: $tp\nRAW: ${showRaw(tp)}")
      VerboseTraversal.incIdent
      val tpCalc = tp match {
        ////////////////////////////////////////////////////////////////////////
        // Value cases
        ////////////////////////////////////////////////////////////////////////
        case ConstantType(ConstantCalc(t)) => Some(t) //Constant
        case OpCalc(t) => Some(t) // Operational Function
        case OpCastCalc(t) => Some(t) //Op Cast wrappers
        case TwoFaceCalc(t) => Some(t) //TwoFace values
        case NonLiteralCalc(t) => Some(t)// Non-literal values
        case NatCalc(t) => Some(t) //For Shapeless Nat
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // Tree traversal
        ////////////////////////////////////////////////////////////////////////
        case tp @ ExistentialType(_, _) => unapply(tp.underlying)
        case TypeBounds(lo, hi) => unapply(hi) match {
          case Some(t : CalcLit) => Some(t)
          //There can be cases, like in the following example, where we can extract a non-literal value.
          //  def foo2[W](w : TwoFace.Int[W])(implicit tfs : TwoFace.Int.Shell1[Negate, W, Int]) = -w+1
          //We want to calculate `-w+1`, even though we have not provided a complete implicit.
          //While returning `TwoFace.Int[Int](-w+1)` is possible in this case, we would rather reserve
          //the ability to have a literal return type, so `TwoFace.Int[Negate[W]+1](-w+1)` is returned.
          //So even if we can have a `Some(CalcType)` returning, we force it as an upper-bound calc type.
          case Some(t) => Some(CalcType.UB(t))
          case _ => None
        }
        case RefinedType(parents, scope) =>
          parents.iterator map unapply collectFirst { case Some(x) => x }
        case NullaryMethodType(tpe) => unapply(tpe)
        case TypeRef(_, sym, _) if sym.isAliasType =>
          val tpDealias = tp.dealias
          if (tpDealias == tp)
            abort("Unable to dealias type: " + showRaw(tp))
          else
            unapply(tpDealias)
        case TypeRef(pre, sym, Nil) => unapply(sym.info asSeenFrom (pre, sym.owner))
        case SingleType(pre, sym) => unapply(sym.info asSeenFrom (pre, sym.owner))
        ////////////////////////////////////////////////////////////////////////

        case _ =>
          VerboseTraversal("Exhausted search at: " + showRaw(tp))
          None
      }
      VerboseTraversal.decIdent
      tpCalc
    }
  }
  ////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Calculates from a constant
  ////////////////////////////////////////////////////////////////////////
  object ConstantCalc {
    def unapply(constant: Constant): Option[CalcLit] = {
      constant match {
        case Constant(t : Char) => Some(CalcLit(t))
        case Constant(t : Int) => Some(CalcLit(t))
        case Constant(t : Long) => Some(CalcLit(t))
        case Constant(t : Float) => Some(CalcLit(t))
        case Constant(t : Double) => Some(CalcLit(t))
        case Constant(t : String) => Some(CalcLit(t))
        case Constant(t : Boolean) => Some(CalcLit(t))
        case _ => None
      }
    }
  }
  ////////////////////////////////////////////////////////////////////////

  def abort(msg: String, annotatedSym : Option[TypeSymbol] = defaultAnnotatedSym, position : Position = c.enclosingPosition): Nothing = {
    VerboseTraversal(s"!!!!!!aborted with: $msg at $annotatedSym, $defaultAnnotatedSym")
    if (annotatedSym.isDefined) setAnnotation(msg, annotatedSym.get)
    MacroCache.setErrorCache(msg) //propagating the error in case this is an inner implicit call for OpIntercept
    c.abort(position, msg)
  }

  def buildWarningMsgLoc : String = s"${c.enclosingPosition.source.path}:${c.enclosingPosition.line}:${c.enclosingPosition.column}"
  def buildWarningMsg(msg: String): String = s"Warning: $buildWarningMsgLoc    $msg"
  def buildWarningMsg(msg: Tree): Tree = q""" "Warning: " + $buildWarningMsgLoc + "    " + $msg """

  def constantTreeOf(t : Any) : Tree = Literal(Constant(t))

  def constantTypeOf(t: Any) : Type = c.internal.constantType(Constant(t))

  def genOpTreeLit(opTpe : Type, t: Any) : Tree = {
    val outTpe = constantTypeOf(t)
    val outTree = constantTreeOf(t)
    val outWideTpe = outTpe.widen
    val outTypeName = TypeName("Out" + wideTypeName(outTpe))
    val outWideLiteral = outTree
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
    val outWideLiteral = constantTreeOf(t)
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

  def genOpTreeNLit(opTpe : Type, calc : CalcNLit) : Tree = {
    val valueTree = calc.tree
    val outTpe = calc.tpe
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

  def genOpTreeUnknown(opTpe : Type, calc : CalcUnknown) : Tree = {
    val outTpe = calc.tpe
    calc.treeOption match {
      case Some(valueTree) =>
        q"""
          new $opTpe {
            type OutWide = $outTpe
            type Out = $outTpe
            final lazy val value: $outTpe = $valueTree
            final val isLiteral = false
            final lazy val valueWide: $outTpe = $valueTree
          }
        """
      case None =>
        q"""
          new $opTpe {
            type OutWide = $outTpe
            type Out = $outTpe
            final lazy val value: $outTpe = throw new IllegalArgumentException("This operation does not produce a value.")
            final val isLiteral = false
            final lazy val valueWide: $outTpe = throw new IllegalArgumentException("This operation does not produce a value.")
          }
        """
    }
  }

  def extractionFailed(tpe: Type) = {
    val msg = s"Cannot extract value from $tpe\n" + "showRaw==> " + showRaw(tpe)
    abort(msg)
  }
  def extractionFailed(tree: Tree) = {
    val msg = s"Cannot extract value from $tree\n" + "showRaw==> " + showRaw(tree)
    abort(msg)
  }

  def extractValueFromOpTree(opTree : c.Tree) : CalcVal = {
    def outFindCond(elem : c.Tree) : Boolean = elem match {
      case q"final val value : $valueTpe = $valueTree" => true
      case _ => false
    }
    def getOut(opClsBlk : List[c.Tree]) : CalcVal = opClsBlk.find(outFindCond) match {
      case Some(q"final val value : $valueTpe = $valueTree") =>
        valueTree match {
          case Literal(ConstantCalc(t)) => t
          case _ => valueTpe match {
            case NonLiteralCalc(t) => CalcNLit(t, q"$valueTree")
            case _ => extractionFailed(opTree)
          }
        }
      case _ => extractionFailed(opTree)
    }

    opTree match {
        case q"""{
                $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends ..$parents { $self => ..$opClsBlk }
                $expr(...$exprss)
              }""" => getOut(opClsBlk)
        case _ => extractionFailed(opTree)
    }
  }

  def extractValueFromNumTree(numValueTree : c.Tree) : CalcVal = {
    val typedTree = c.typecheck(numValueTree)
    TypeCalc(typedTree.tpe) match {
      case t : CalcLit => t
      case t : CalcType.UB => CalcNLit(t, numValueTree, typedTree.tpe)
      case t : CalcNLit => CalcNLit(t, numValueTree)
      case _ => extractionFailed(typedTree.tpe)
    }
  }

  def extractValueFromTwoFaceTree(tfTree : c.Tree) : CalcVal = {
    val typedTree = c.typecheck(tfTree)
    TypeCalc(typedTree.tpe) match {
      case t : CalcLit => t
      case t : CalcType => CalcNLit(t, q"$tfTree.getValue")
      case t : CalcNLit => CalcNLit(t, q"$tfTree.getValue")
      case t =>
//        println(t)
        extractionFailed(typedTree.tpe)
    }
  }

  def wideTypeName(tpe : Type) : String = tpe.widen.typeSymbol.name.toString

  object HasOutValue {
    def unapply(tree : Tree) : Option[Tree] = tree match {
      case Apply(Apply(_,_), List(Block(ClassDef(_,_,_,Template(_,_,members)) :: _, _))) =>
        members.collectFirst {
          case ValDef(_,TermName("value "),_,t) => t
        }
      case _ => None
    }
  }

  object GetArgTree {
    def isMethodMacroCall : Boolean = c.enclosingImplicits.last.sym.isMacro
    def getAllArgs(tree : Tree, lhs : Boolean) : List[Tree] = tree match {
      case ValDef(_,_,_,Apply(_, t)) => t
      case HasOutValue(valueTree) => List(valueTree)
      case Apply(TypeApply(_,_), List(HasOutValue(valueTree))) => List(valueTree)
      case Apply(Apply(_,_), _) => getAllArgsRecur(tree)
      case Apply(TypeApply(_,_), _) => getAllArgsRecur(tree)
      case Apply(_, args) => if (isMethodMacroCall || lhs) args else List(tree)
      case t : Select => List(t)
      case t : Literal => List(t)
      case t : Ident => List(t)
      case _ => getAllArgsRecur(tree)
    }

    def getAllArgsRecur(tree : Tree) : List[Tree] = tree match {
      case Apply(fun, args) => getAllArgsRecur(fun) ++ args
      case _ => List()
    }

    def getAllLHSArgs(tree : Tree) : List[Tree] = tree match {
      case Apply(TypeApply(Select(t, _), _), _) => getAllArgs(t, true)
      case TypeApply(Select(t, _), _) => getAllArgs(t, true)
      case Select(t, _) => getAllArgs(t, true)
      case _ => abort("Left-hand-side tree not found")
    }

    private var argListUsed = false
    private lazy val argList = {
      argListUsed = true
      getAllArgs(c.enclosingImplicits.last.tree, false)
    }
    private var lhsArgListUsed = false
    private lazy val lhsArgList = {
      lhsArgListUsed = true
      getAllLHSArgs(c.enclosingImplicits.last.tree)
    }
    def argContext : List[Tree] = (argListUsed, lhsArgListUsed) match {
      case (false, false) => List()
      case (true, false) => argList
      case (false, true) => lhsArgList
      case (true, true) => argList ++ lhsArgList
    }
    def apply(argIdx : Int, lhs : Boolean) : (Tree, Type) = {
      val tree = c.enclosingImplicits.last.tree
//      println(">>>>>>> enclosingImpl: ")// + c.enclosingImplicits.last)
//      println("pt: " + c.enclosingImplicits.last.pt)
//      println("tree: " + c.enclosingImplicits.last.tree)
//      println("rawTree: " + showRaw(c.enclosingImplicits.last.tree))
      val allArgs = if (lhs) lhsArgList else argList
//      println("args: " + allArgs)
//      println("<<<<<<< rawArgs" + showRaw(allArgs))

      val argTree : Tree = if (argIdx < allArgs.length) c.typecheck(allArgs(argIdx))
      else abort(s"Argument index($argIdx) is not smaller than the total number of arguments(${allArgs.length})")

      val tpe = c.enclosingImplicits.last.pt match {
        case TypeRef(_,sym,tp :: _) if (sym == func1Sym) => tp //conversion, so get the type from last.pt
        case _ => argTree.tpe //not a conversion, so get the type from the tree
      }
      (argTree, tpe)
    }
  }

  def extractFromArg(argIdx : Int, lhs : Boolean) : Calc = {
    val (typedTree, tpe) = GetArgTree(argIdx, lhs)
    VerboseTraversal(s"@@extractFromArg@@\nTP: $tpe\nRAW: ${showRaw(tpe)}\nTree: $typedTree")
    TypeCalc(tpe) match {
      case _ : CalcUnknown => CalcUnknown(tpe, Some(c.untypecheck(typedTree)), opIntercept = false)
      case t : CalcNLit => CalcNLit(t, typedTree)
      case t => t
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Three operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOpGen[F](implicit ev0: c.WeakTypeTag[F]): MaterializeOpAuxGen =
    new MaterializeOpAuxGen(weakTypeOf[F])

  def opCalc(funcType : TypeSymbol, aCalc : => Calc, bCalc : => Calc, cCalc : => Calc) : Calc = {
    lazy val a = aCalc
    lazy val b = bCalc
    lazy val cArg = cCalc
    def unsupported() : Calc = {
      val opMacroTpe = typeOf[OpMacro[_,_,_,_]].typeConstructor
      val opTpe = appliedType(opMacroTpe, List(funcType.toType, a.tpe, b.tpe, cArg.tpe))
      val interceptTpe = typeOf[singleton.ops.OpIntercept[_]].typeConstructor
      MacroCache.clearErrorCache()
      try {
        val itree = c.inferImplicitValue (
          appliedType(interceptTpe, List(opTpe)),
          silent = false
        )
        TypeCalc(itree.tpe.decls.head.info) match {
          case t : CalcUnknown => t.copy(treeOption = Some(c.untypecheck(q"$itree.value")),opIntercept = true) //the unknown result must be marked properly so we allow it later
          case t => t
        }
      } catch {
        case TypecheckException(_, _) =>
          MacroCache.getErrorMessage match {
            case m if m.nonEmpty => abort(m)
            case _ => (a, b) match {
              case (_ : CalcVal, _ : CalcVal) => abort(s"Unsupported operation $opTpe")
              case _ => CalcUnknown(funcType.toType, None, opIntercept = false)
            }
          }
      }
    }

    //The output val is literal if all arguments are literal. Otherwise, it is non-literal.
    lazy implicit val cvKind : CalcVal.Kind = (a, b, cArg) match {
      case (_ : CalcLit, _ : CalcLit, _ : CalcLit) => CalcVal.Lit
      case _ => CalcVal.NLit
    }

    def AcceptNonLiteral : Calc = Id //AcceptNonLiteral has a special handling in MaterializeOpAuxGen
    def GetArg : Calc = (a, b) match {
      case (CalcLit.Int(idx), CalcLit.Boolean(lhs)) if (idx >= 0) => extractFromArg(idx, lhs)
      case _ => unsupported()
    }
    def Id : Calc = a match {
      case (av : Calc) => av
      case _ => unsupported()
    }
    def ToNat : Calc = ToInt //Same handling, but also has a special case to handle this in MaterializeOpAuxGen
    def ToChar : Calc = a match {
      case CalcVal(t : Char, tt) => CalcVal(t, q"$tt")
      case CalcVal(t : Int, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal(t : Long, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal(t : Float, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal(t : Double, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case _ => unsupported()
    }
    def ToInt : Calc = a match {
      case CalcVal(t : Char, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal(t : Int, tt) => CalcVal(t, q"$tt")
      case CalcVal(t : Long, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal(t : Float, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal(t : Double, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal(t : String, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case _ => unsupported()
    }
    def ToLong : Calc = a match {
      case CalcVal(t : Char, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal(t : Int, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal(t : Long, tt) => CalcVal(t, q"$tt")
      case CalcVal(t : Float, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal(t : Double, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal(t : String, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case _ => unsupported()
    }
    def ToFloat : Calc = a match {
      case CalcVal(t : Char, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal(t : Int, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal(t : Long, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal(t : Float, tt) => CalcVal(t, q"$tt")
      case CalcVal(t : Double, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal(t : String, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case _ => unsupported()
    }
    def ToDouble : Calc = a match {
      case CalcVal(t : Char, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal(t : Int, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal(t : Long, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal(t : Float, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal(t : Double, tt) => CalcVal(t, q"$tt")
      case CalcVal(t : String, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case _ => unsupported()
    }
    def ToString : Calc = a match {
      case CalcVal(t : Char, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal(t : Int, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal(t : Long, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal(t : Float, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal(t : Double, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal(t : String, tt) => CalcVal(t, q"$tt")
      case CalcVal(t : Boolean, tt) => CalcVal(t.toString, q"$tt.toString")
      case _ => unsupported()
    }
    def Negate : Calc = a match {
      case CalcVal(t : Char, tt) => CalcVal(-t, q"-$tt")
      case CalcVal(t : Int, tt) => CalcVal(-t, q"-$tt")
      case CalcVal(t : Long, tt) => CalcVal(-t, q"-$tt")
      case CalcVal(t : Float, tt) => CalcVal(-t, q"-$tt")
      case CalcVal(t : Double, tt) => CalcVal(-t, q"-$tt")
      case _ => unsupported()
    }
    def Abs : Calc = a match {
      case CalcVal(t : Int, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case CalcVal(t : Long, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case CalcVal(t : Float, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case CalcVal(t : Double, tt) => CalcVal(math.abs(t), q"_root_.scala.math.abs($tt)")
      case _ => unsupported()
    }
    def NumberOfLeadingZeros : Calc = a match {
      case CalcVal(t : Int, tt) => CalcVal(nlz(t), q"_root_.singleton.ops.impl.nlz($tt)")
      case CalcVal(t : Long, tt) => CalcVal(nlz(t), q"_root_.singleton.ops.impl.nlz($tt)")
      case _ => unsupported()
    }
    def Floor : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal(math.floor(t), q"_root_.scala.math.floor($tt)")
      case _ => unsupported()
    }
    def Ceil : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal(math.ceil(t), q"_root_.scala.math.ceil($tt)")
      case _ => unsupported()
    }
    def Round : Calc = a match {
      case CalcVal(t : Float, tt) => CalcVal(math.round(t), q"_root_.scala.math.round($tt)")
      case CalcVal(t : Double, tt) => CalcVal(math.round(t), q"_root_.scala.math.round($tt)")
      case _ => unsupported()
    }
    def Sin : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal(math.sin(t), q"_root_.scala.math.sin($tt)")
      case _ => unsupported()
    }
    def Cos : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal(math.cos(t), q"_root_.scala.math.cos($tt)")
      case _ => unsupported()
    }
    def Tan : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal.mayFail(Primitive.Double, math.tan(t), q"_root_.scala.math.tan($tt)")
      case _ => unsupported()
    }
    def Sqrt : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal.mayFail(Primitive.Double, math.sqrt(t), q"_root_.scala.math.sqrt($tt)")
      case _ => unsupported()
    }
    def Log : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal.mayFail(Primitive.Double, math.log(t), q"_root_.scala.math.log($tt)")
      case _ => unsupported()
    }
    def Log10 : Calc = a match {
      case CalcVal(t : Double, tt) => CalcVal.mayFail(Primitive.Double, math.log10(t), q"_root_.scala.math.log10($tt)")
      case _ => unsupported()
    }
    def Reverse : Calc = a match {
      case CalcVal(t : String, tt) => CalcVal(t.reverse, q"$tt.reverse")
      case _ => unsupported()
    }
    def Not : Calc = a match {
      case CalcVal(t : Boolean, tt) => CalcVal(!t, q"!$tt")
      case _ => unsupported()
    }
    def Require : Calc = a match {
      case CalcLit.Boolean(true) => CalcLit(true)
      case CalcLit.Boolean(false) => b match {
        case CalcLit.String(msg) =>
          if (cArg.tpe.typeSymbol == symbolOf[Warn]) {
            println(buildWarningMsg(msg))
            CalcLit(false)
          } else if (cArg.tpe.typeSymbol == symbolOf[NoSym]) {
            abort(msg)
          } else {
            //redirection of implicit not found annotation is required to the given symbol
            abort(msg, Some(cArg.tpe.typeSymbol.asType))
          }
        //directly using the java lib `require` resulted in compiler crash, so we use wrapped require instead
        case CalcNLit(Primitive.String, msg, _) => cArg match {
          case CalcUnknown(t, _, false) if t.typeSymbol == symbolOf[Warn] =>
            CalcNLit(Primitive.Boolean, q"""{println(${buildWarningMsg(msg)}); false}""")
          case _ =>
            CalcNLit(Primitive.Boolean, q"{_root_.singleton.ops.impl._require(false, $msg); false}")
        }
        case _ => unsupported()
      }
      case CalcNLit(Primitive.Boolean, cond, _) => b match {
        //directly using the java lib `require` resulted in compiler crash, so we use wrapped require instead
        case CalcVal(msg : String, msgt) => cArg match {
          case CalcUnknown(t, _, false) if t == symbolOf[Warn] =>
            CalcNLit(Primitive.Boolean,
              q"""{
                  if ($cond) true
                  else {
                    println(${buildWarningMsg(msgt)})
                    false
                  }
                }""")
          case _ =>
            CalcNLit(Primitive.Boolean, q"{_root_.singleton.ops.impl._require($cond, $msgt); true}")
        }
        case _ => unsupported()
      }
      case _ => unsupported()
    }
    def ITE : Calc = (a, b, cArg) match {
      //Also has special case handling inside unapply
      case (CalcVal(it : Boolean,itt), CalcVal(tt : Char,ttt), CalcVal(et : Char,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal(it : Boolean,itt), CalcVal(tt : Int,ttt), CalcVal(et : Int,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal(it : Boolean,itt), CalcVal(tt : Long,ttt), CalcVal(et : Long,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal(it : Boolean,itt), CalcVal(tt : Float,ttt), CalcVal(et : Float,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal(it : Boolean,itt), CalcVal(tt : Double,ttt), CalcVal(et : Double,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal(it : Boolean,itt), CalcVal(tt : String,ttt), CalcVal(et : String,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal(it : Boolean,itt), CalcVal(tt : Boolean,ttt), CalcVal(et : Boolean,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case _ => unsupported()
    }
    def Next : Calc = b match {
      case (bv : CalcVal) => bv
      case _ => unsupported()
    }
    def Plus : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) => CalcVal(at + bt, q"$att + $btt")
      case _ => unsupported()
    }
    def Minus : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at - bt, q"$att - $btt")
      case _ => unsupported()
    }
    def Mul : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at * bt, q"$att * $btt")
      case _ => unsupported()
    }
    def Div : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal.mayFail(Primitive.Int, at / bt, q"$att / $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal.mayFail(Primitive.Int, at / bt, q"$att / $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal.mayFail(Primitive.Long, at / bt, q"$att / $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal.mayFail(Primitive.Float, at / bt, q"$att / $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal.mayFail(Primitive.Double, at / bt, q"$att / $btt")
      case _ => unsupported()
    }
    def Mod : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal.mayFail(Primitive.Int, at % bt, q"$att % $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal.mayFail(Primitive.Int, at % bt, q"$att % $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal.mayFail(Primitive.Long, at % bt, q"$att % $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal.mayFail(Primitive.Float, at % bt, q"$att % $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal.mayFail(Primitive.Double, at % bt, q"$att % $btt")
      case _ => unsupported()
    }
    def Sml : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at < bt, q"$att < $btt")
      case _ => unsupported()
    }
    def Big : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at > bt, q"$att > $btt")
      case _ => unsupported()
    }
    def SmlEq : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case _ => unsupported()
    }
    def BigEq : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case _ => unsupported()
    }
    def Eq : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal(at : Boolean, att), CalcVal(bt : Boolean, btt)) => CalcVal(at == bt, q"$att == $btt")
      case _ => unsupported()
    }
    def Neq : Calc = (a, b) match {
      case (CalcVal(at : Char, att), CalcVal(bt : Char, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal(at : Boolean, att), CalcVal(bt : Boolean, btt)) => CalcVal(at != bt, q"$att != $btt")
      case _ => unsupported()
    }
    def And : Calc = a match {
      case CalcLit.Boolean(ab) => //`And` expressions where the LHS is a literal can be inlined
        if (ab) b match {
          case CalcVal(_ : Boolean,_) => b //inlining the value of RHS when the LHS is true
          case _ => unsupported()
        } else CalcLit(false) //inlining as false when the LHS is false
      case _ => (a, b) match {
        case (CalcVal(at : Boolean, att), CalcVal(bt : Boolean, btt)) => CalcVal(at && bt, q"$att && $btt")
        case _ => unsupported()
      }
    }
    def Or : Calc = a match {
      case CalcLit.Boolean(ab) => //`Or` expressions where the LHS is a literal can be inlined
        if (!ab) b match {
          case CalcVal(_ : Boolean,_) => b //inlining the value of RHS when the LHS is false
          case _ => unsupported()
        } else CalcLit(true) //inlining as true when the LHS is true
      case _ => (a, b) match {
        case (CalcVal(at : Boolean, att), CalcVal(bt : Boolean, btt)) => CalcVal(at || bt, q"$att || $btt")
        case _ => unsupported()
      }
    }
    def Pow : Calc = (a, b) match {
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) =>
        CalcVal(math.pow(at.toDouble, bt.toDouble), q"_root_.scala.math.pow($att.toDouble, $btt.toDouble)")
      case _ => unsupported()
    }
    def Min : Calc = (a, b) match {
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) =>
        CalcVal(math.min(at, bt), q"_root_.scala.math.min($att, $btt)")
      case _ => unsupported()
    }
    def Max : Calc = (a, b) match {
      case (CalcVal(at : Int, att), CalcVal(bt : Int, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case (CalcVal(at : Long, att), CalcVal(bt : Long, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case (CalcVal(at : Float, att), CalcVal(bt : Float, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case (CalcVal(at : Double, att), CalcVal(bt : Double, btt)) =>
        CalcVal(math.max(at, bt), q"_root_.scala.math.max($att, $btt)")
      case _ => unsupported()
    }
    def Substring : Calc = (a, b) match {
      case (CalcVal(at : String, att), CalcVal(bt : Int, btt)) =>
        CalcVal.mayFail(Primitive.String, at.substring(bt), q"$att.substring($btt)")
      case _ => unsupported()
    }
    def SubSequence : Calc = (a, b, cArg) match {
      case (CalcVal(at : String, att), CalcVal(bt : Int, btt), CalcVal(ct : Int, ctt)) =>
        CalcVal.mayFail(Primitive.String, at.subSequence(bt, ct), q"$att.subSequence($btt, $ctt)")
      case _ => unsupported()
    }
    def StartsWith : Calc = (a, b) match {
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) =>
        CalcVal(at.startsWith(bt), q"$att.startsWith($btt)")
      case _ => unsupported()
    }
    def EndsWith : Calc = (a, b) match {
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) =>
        CalcVal(at.endsWith(bt), q"$att.endsWith($btt)")
      case _ => unsupported()
    }
    def Head : Calc = a match {
      case CalcVal(at : String, att) =>
        CalcVal.mayFail(Primitive.Char, at.head, q"$att.head")
      case _ => unsupported()
    }
    def Tail : Calc = a match {
      case CalcVal(at : String, att) => CalcVal(at.tail, q"$att.tail")
      case _ => unsupported()
    }
    def CharAt : Calc = (a, b) match {
      case (CalcVal(at : String, att), CalcVal(bt : Int, btt)) =>
        CalcVal.mayFail(Primitive.Char, at.charAt(bt), q"$att.charAt($btt)")
      case _ => unsupported()
    }
    def Length : Calc = a match {
      case CalcVal(at : String, att) => CalcVal(at.length, q"$att.length")
      case _ => unsupported()
    }
    def Matches : Calc = (a, b) match {
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) =>
        CalcVal.mayFail(Primitive.Boolean, at.matches(bt), q"$att.matches($btt)")
      case _ => unsupported()
    }
    def FirstMatch : Calc = (a, b) match {
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) =>
        CalcVal.mayFail(Primitive.String, bt.r.findFirstIn(at).get, q"$btt.r.findFirstIn($att).get")
      case _ => unsupported()
    }
    def PrefixMatch : Calc = (a, b) match {
      case (CalcVal(at : String, att), CalcVal(bt : String, btt)) =>
        CalcVal.mayFail(Primitive.String, bt.r.findPrefixOf(at).get, q"$btt.r.findPrefixOf($att).get")
      case _ => unsupported()
    }
    def ReplaceFirstMatch : Calc = (a, b, cArg) match {
      case (CalcVal(at : String, att), CalcVal(bt : String, btt), CalcVal(ct : String, ctt)) =>
        CalcVal.mayFail(Primitive.String, bt.r.replaceFirstIn(at, ct), q"$btt.r.replaceFirstIn($att, $ctt)")
      case _ => unsupported()
    }
    def ReplaceAllMatches : Calc = (a, b, cArg) match {
      case (CalcVal(at : String, att), CalcVal(bt : String, btt), CalcVal(ct : String, ctt)) =>
        CalcVal.mayFail(Primitive.String, bt.r.replaceAllIn(at, ct), q"$btt.r.replaceAllIn($att, $ctt)")
      case _ => unsupported()
    }

    funcType match {
      case funcTypes.AcceptNonLiteral => AcceptNonLiteral
      case funcTypes.GetArg => GetArg
      case funcTypes.Id => Id
      case funcTypes.ToNat => ToNat
      case funcTypes.ToChar => ToChar
      case funcTypes.ToInt => ToInt
      case funcTypes.ToLong => ToLong
      case funcTypes.ToFloat => ToFloat
      case funcTypes.ToDouble => ToDouble
      case funcTypes.ToString => ToString
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
      case funcTypes.SubSequence => SubSequence
      case funcTypes.StartsWith => StartsWith
      case funcTypes.EndsWith => EndsWith
      case funcTypes.Head => Head
      case funcTypes.Tail => Tail
      case funcTypes.CharAt => CharAt
      case funcTypes.Length => Length
      case funcTypes.Matches => Matches
      case funcTypes.FirstMatch => FirstMatch
      case funcTypes.PrefixMatch => PrefixMatch
      case funcTypes.ReplaceFirstMatch => ReplaceFirstMatch
      case funcTypes.ReplaceAllMatches => ReplaceAllMatches
      case _ => unsupported()
    }
  }

  final class MaterializeOpAuxGen(opTpe: Type) {
    def usingFuncName : Tree = {
      val funcType = opTpe.typeArgs.head.typeSymbol.asType
      val opResult = TypeCalc(opTpe)

      val genTree = (funcType, opResult) match {
        case (funcTypes.ToNat, CalcLit.Int(t)) =>
          if (t < 0) abort(s"Nat cannot be a negative literal. Found: $t")
          else genOpTreeNat(opTpe, t)
        case (_, CalcLit(_, t)) => genOpTreeLit(opTpe, t)
        case (funcTypes.AcceptNonLiteral | funcTypes.GetArg, t : CalcNLit) => genOpTreeNLit(opTpe, t)
        case (_, t @ CalcUnknown(_,_,true)) => genOpTreeUnknown(opTpe, t)
        case (funcTypes.GetArg, t : CalcUnknown) => genOpTreeUnknown(opTpe, t)
        case (_, t: CalcNLit) =>
          abort("Calculation has returned a non-literal type/value.\nTo accept non-literal values, use `AcceptNonLiteral[T]`.")
        case _ => extractionFailed(opTpe)
      }

//      println(genTree)
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////
  // TwoFace Shell
  ///////////////////////////////////////////////////////////////////////////////////////////
  def TwoFaceShellMaterializer[Shell](implicit shell : c.WeakTypeTag[Shell])
  : TwoFaceShellMaterializer[Shell] = new TwoFaceShellMaterializer[Shell](weakTypeOf[Shell])

  final class TwoFaceShellMaterializer[Shell](shellTpe : Type) {
    def shell(shellAliasTpe : TypeSymbol) : c.Tree = {
      val owner = c.internal.enclosingOwner
      if (owner.asTerm.name.toString == "equals" && owner.owner.isClass && owner.owner.asClass.isCaseClass) {
        abort("A case class equals workaround is required. See https://github.com/scala/bug/issues/10536")
      }

      val funcApplyTpe = shellTpe.typeArgs(0)
      val funcArgsTpe = shellTpe.typeArgs(1)
      val (tfValueTree, tfName) = TypeCalc(funcArgsTpe) match {
        case (t: CalcVal) => (t.tree, t.name)
        case _ => extractionFailed(shellTpe)
      }
      val tfTerm = TermName(tfName)
      val tfType = TypeName(tfName)
      val outTpe = TypeCalc(funcApplyTpe).tpe
      val paramVec = for (i <- 4 to shellTpe.typeArgs.length by 2; typeTree = AppliedTypeTree(Ident(TypeName("<byname>")), List(tq"${shellTpe.typeArgs(i-1)}")))
        yield ValDef(Modifiers(Flag.PARAM | Flag.BYNAMEPARAM),TermName(s"arg${(i-4)/2+1}"),typeTree,EmptyTree)

      val paramTree = List(paramVec.toList)
      val genTree =
       q"""
         new $shellTpe {
           type Out = $outTpe
           def apply(...$paramTree) : _root_.singleton.twoface.TwoFace.$tfType[$outTpe] = {
             _root_.singleton.twoface.TwoFace.$tfTerm.create[$outTpe]($tfValueTree)
           }
         }
        """
//      println(showCode(genTree))
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////
  // TwoFace
  ///////////////////////////////////////////////////////////////////////////////////////////
  def TwoFaceMaterializer : TwoFaceMaterializer = new TwoFaceMaterializer

  final class TwoFaceMaterializer {
    def genTwoFace(outTpe : Type, outTree : Tree, tfName : String) : c.Tree = {
      val tfTerm = TermName(tfName)
      q"""
        _root_.singleton.twoface.TwoFace.$tfTerm.create[$outTpe]($outTree)
      """
    }
    def genTwoFace(calc : CalcVal) : c.Tree = {
      genTwoFace(calc.tpe, calc.tree, calc.name)
    }
    def fromNumValue(numValueTree : c.Tree, tfSym : TypeSymbol) : c.Tree =  {
//      println(tfSym.name)
      val genTree = genTwoFace(extractValueFromNumTree(numValueTree))
//      println(genTree)
      genTree
    }
    def toNumValue(tfTree : c.Tree, tfSym : TypeSymbol, tTpe : Type) : c.Tree = {
      val calc = extractValueFromTwoFaceTree(tfTree)
      val outTpe = calc.tpe
      val outTree = calc.tree
      val genTree =
        q"""
          $outTree.asInstanceOf[$tTpe]
        """
//      println(genTree)
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////
  // Checked0Param TwoFace
  ///////////////////////////////////////////////////////////////////////////////////////////
  def Checked0ParamMaterializer[Chk, Cond, Msg, T](implicit chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T]) :
  Checked0ParamMaterializer[Chk, Cond, Msg, T] = new Checked0ParamMaterializer[Chk, Cond, Msg, T](symbolOf[Chk], weakTypeOf[Cond], weakTypeOf[Msg], weakTypeOf[T])

  final class Checked0ParamMaterializer[Chk, Cond, Msg, T](chkSym : TypeSymbol, condTpe : Type, msgTpe : Type, tTpe : Type) {
    def newChecked(calc : CalcVal, chkArgTpe : Type) : c.Tree = {
      val outTpe = calc.tpe
      val outTree = calc.tree
      val outTpeWide = outTpe.widen

      val fixedCondTpe = appliedType(condTpe.typeConstructor, outTpe).dealias
      val fixedMsgTpe = appliedType(msgTpe.typeConstructor, outTpe).dealias

      val condCalc = TypeCalc(fixedCondTpe) match {
        case t : CalcVal => t
        case _ => extractionFailed(fixedCondTpe)
      }

      val msgCalc = condCalc match {
        case (CalcLit.Boolean(true)) => CalcLit("") //Not calculating message if condition is constant true
        case _ => TypeCalc(fixedMsgTpe) match {
          case t : CalcVal => t
          case _ => extractionFailed(fixedMsgTpe)
        }
      }

      val reqCalc = opCalc(funcTypes.Require, condCalc, msgCalc, CalcUnknown(typeOf[NoSym], None, opIntercept = false))

      q"""
         (new $chkSym[$condTpe, $msgTpe, $chkArgTpe]($outTree.asInstanceOf[$outTpe]))
       """
    }
    def newChecked(calc : CalcVal) : c.Tree = newChecked(calc, calc.tpe)
    def fromOpImpl(opTree : c.Tree) : c.Tree = {
      val numValueCalc = extractValueFromOpTree(opTree)
      val genTree = newChecked(numValueCalc, tTpe)
//      println(genTree)
      genTree
    }
    def fromNumValue(numValueTree : c.Tree) : c.Tree = {
      val numValueCalc = extractValueFromNumTree(numValueTree)
      val genTree = newChecked(numValueCalc)
//      println(genTree)
      genTree
    }
    def fromTF(tfTree : c.Tree) : c.Tree = {
      val tfValueCalc = extractValueFromTwoFaceTree(tfTree)
      val genTree = newChecked(tfValueCalc)
//      println(genTree)
      genTree
    }
    def widen(chkTree : c.Tree) : c.Tree = {
      val tfValueCalc = extractValueFromTwoFaceTree(chkTree)
      val genTree = newChecked(tfValueCalc, tTpe)
      //      println(genTree)
      genTree
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////
  // Checked1Param TwoFace
  ///////////////////////////////////////////////////////////////////////////////////////////
  def Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param](implicit chk : c.WeakTypeTag[Chk], cond : c.WeakTypeTag[Cond], msg : c.WeakTypeTag[Msg], t : c.WeakTypeTag[T], paramFace : c.WeakTypeTag[ParamFace], p : c.WeakTypeTag[Param]) :
  Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param] = new Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param](symbolOf[Chk], weakTypeOf[Cond], weakTypeOf[Msg], weakTypeOf[T], weakTypeOf[ParamFace], weakTypeOf[Param])

  final class Checked1ParamMaterializer[Chk, Cond, Msg, T, ParamFace, Param](chkSym : TypeSymbol, condTpe : Type, msgTpe : Type, tTpe : Type, paramFaceTpe : Type, paramTpe : Type) {
    def newChecked(tCalc : CalcVal, chkArgTpe : Type) : c.Tree = {
      val outTpe = tCalc.tpe
      val outTree = tCalc.tree
      val paramCalc = TypeCalc(paramTpe) match {
        case t : CalcVal => t
        case _ => extractionFailed(paramTpe)
      }

      val fixedCondTpe = appliedType(condTpe.typeConstructor, tCalc.tpe, paramCalc.tpe).dealias
      val fixedMsgTpe = appliedType(msgTpe.typeConstructor, tCalc.tpe, paramCalc.tpe).dealias

      val condCalc = TypeCalc(fixedCondTpe) match {
        case t : CalcVal => t
        case _ => extractionFailed(fixedCondTpe)
      }

      val msgCalc = condCalc match {
        case (CalcLit.Boolean(true)) => CalcLit("") //Not calculating message if condition is constant true
        case _ => TypeCalc(fixedMsgTpe) match {
          case t : CalcVal => t
          case _ => extractionFailed(fixedMsgTpe)
        }
      }

      val reqCalc = opCalc(funcTypes.Require, condCalc, msgCalc, CalcUnknown(typeOf[NoSym], None, opIntercept = false))

      q"""
         (new $chkSym[$condTpe, $msgTpe, $chkArgTpe, $paramFaceTpe, $paramTpe]($outTree.asInstanceOf[$outTpe]))
       """
    }
    def newChecked(tCalc : CalcVal) : c.Tree =
      newChecked(tCalc, tCalc.tpe)
    def fromOpImpl(tOpTree : c.Tree) : c.Tree = {
      val tCalc = extractValueFromOpTree(tOpTree)
      val genTree = newChecked(tCalc, tTpe)
//      println(genTree)
      genTree
    }
    def fromNumValue(tNumTree : c.Tree) : c.Tree = {
      val tCalc = extractValueFromNumTree(tNumTree)
      val genTree = newChecked(tCalc)
//      println(genTree)
      genTree
    }
    def fromTF(tTFTree : c.Tree) : c.Tree = {
      val tCalc = extractValueFromTwoFaceTree(tTFTree)
      val genTree = newChecked(tCalc)
//      println(genTree)
      genTree
    }
    def widen(tTFTree : c.Tree) : c.Tree = {
      val tCalc = extractValueFromTwoFaceTree(tTFTree)
      val genTree = newChecked(tCalc, tTpe)
      //      println(genTree)
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
