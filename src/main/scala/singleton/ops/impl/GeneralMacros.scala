package singleton.ops.impl
import macrocompat.bundle
import shapeless.tag
import shapeless.tag.@@
import singleton.twoface.impl.TwoFaceAny

import scala.reflect.macros.whitebox
@bundle
trait GeneralMacros {
  val c: whitebox.Context

  import c.universe._

  object funcTypes {
    val Arg = symbolOf[OpId.Arg]
    val AcceptNonLiteral = symbolOf[OpId.AcceptNonLiteral]
    val GetArg = symbolOf[OpId.GetArg]
    val GetLHSArg = symbolOf[OpId.GetLHSArg]
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
    val ToSymbol = symbolOf[OpId.ToSymbol]
    val IsNat = symbolOf[OpId.IsNat]
    val IsChar = symbolOf[OpId.IsChar]
    val IsInt = symbolOf[OpId.IsInt]
    val IsLong = symbolOf[OpId.IsLong]
    val IsFloat = symbolOf[OpId.IsFloat]
    val IsDouble = symbolOf[OpId.IsDouble]
    val IsString = symbolOf[OpId.IsString]
    val IsBoolean = symbolOf[OpId.IsBoolean]
    val IsSymbol = symbolOf[OpId.IsSymbol]
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
    val name : String
  }

  sealed trait CalcType extends Calc
  object CalcType {
    sealed trait Char extends CalcType{type T = std.Char; val tpe = typeOf[scala.Char]; val name = "Char"}
    sealed trait Int extends CalcType{type T = std.Int; val tpe = typeOf[scala.Int]; val name = "Int"}
    sealed trait Long extends CalcType{type T = std.Long; val tpe = typeOf[scala.Long]; val name = "Long"}
    sealed trait Float extends CalcType{type T = std.Float; val tpe = typeOf[scala.Float]; val name = "Float"}
    sealed trait Double extends CalcType{type T = std.Double; val tpe = typeOf[scala.Double]; val name = "Double"}
    sealed trait String extends CalcType{type T = std.String; val tpe = typeOf[java.lang.String]; val name = "String"}
    sealed trait Boolean extends CalcType{type T = std.Boolean; val tpe = typeOf[scala.Boolean]; val name = "Boolean"}
    sealed trait Symbol extends CalcType{type T = std.Symbol; val tpe = typeOf[scala.Symbol]; val name = "String"}
    object Char extends Char
    object Int extends Int
    object Long extends Long
    object Float extends Float
    object Double extends Double
    object String extends String
    object Boolean extends Boolean
    object Symbol extends Symbol
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
    class Char(val value : std.Char, val tree : Tree) extends CalcVal with CalcType.Char
    object Char {
      def unapply(arg: Char) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Int(val value : std.Int, val tree : Tree) extends CalcVal with CalcType.Int
    object Int {
      def unapply(arg: Int) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Long(val value : std.Long, val tree : Tree) extends CalcVal with CalcType.Long
    object Long {
      def unapply(arg: Long) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Float(val value : std.Float, val tree : Tree) extends CalcVal with CalcType.Float
    object Float {
      def unapply(arg: Float) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Double(val value : std.Double, val tree : Tree) extends CalcVal with CalcType.Double
    object Double {
      def unapply(arg: Double) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class String(val value : std.String, val tree : Tree) extends CalcVal with CalcType.String
    object String {
      def unapply(arg: String) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    class Boolean(val value : std.Boolean, val tree : Tree) extends CalcVal with CalcType.Boolean
    object Boolean {
      def unapply(arg: Boolean) : Option[(arg.T, Tree)] = Some((arg.value, arg.tree))
    }
    def apply[T](value : T, tree : Tree)(implicit unsupported : TypeSymbol, kind : Kind) = kind match {
      case Lit => CalcLit(value)
      case NLit => CalcNLit(value, tree)
    }
  }

  sealed trait CalcLit extends CalcVal {
    override val tpe = constantTypeOf(value)
  }

  object CalcLit {
    implicit val lift = Liftable[CalcLit] { p => p.tree }
    case class Char(override val value : std.Char) extends CalcVal.Char(value, Literal(Constant(value))) with CalcLit
    case class Int(override val value : std.Int) extends CalcVal.Int(value, Literal(Constant(value))) with CalcLit
    case class Long(override val value : std.Long) extends CalcVal.Long(value, Literal(Constant(value))) with CalcLit
    case class Float(override val value : std.Float) extends CalcVal.Float(value, Literal(Constant(value))) with CalcLit
    case class Double(override val value : std.Double) extends CalcVal.Double(value, Literal(Constant(value))) with CalcLit
    case class String(override val value : std.String) extends CalcVal.String(value, Literal(Constant(value))) with CalcLit
    case class Boolean(override val value : std.Boolean) extends CalcVal.Boolean(value, Literal(Constant(value))) with CalcLit
    def apply[T](t : T)(implicit unsupported : TypeSymbol) = t match {
      case t : std.Char => Char(t)
      case t : std.Int => Int(t)
      case t : std.Long => Long(t)
      case t : std.Float => Float(t)
      case t : std.Double => Double(t)
      case t : std.String => String(t)
      case t : std.Boolean => Boolean(t)
      case t : std.Symbol => String(t.name.toString)
      case _ => abort(s"Unsupported literal type: $t")
    }
    def unapply(arg: CalcLit) : Option[arg.T] = Some(arg.value)
  }

  sealed trait CalcTFType extends Calc
  object CalcTFType {
    object Char extends CalcTFType with CalcType.Char
    object Int extends CalcTFType with CalcType.Int
    object Long extends CalcTFType with CalcType.Long
    object Float extends CalcTFType with CalcType.Float
    object Double extends CalcTFType with CalcType.Double
    object String extends CalcTFType with CalcType.String
    object Boolean extends CalcTFType with CalcType.Boolean
  }

  sealed trait CalcUBType extends Calc
  object CalcUBType {
    object Char extends CalcUBType with CalcType.Char
    object Int extends CalcUBType with CalcType.Int
    object Long extends CalcUBType with CalcType.Long
    object Float extends CalcUBType with CalcType.Float
    object Double extends CalcUBType with CalcType.Double
    object String extends CalcUBType with CalcType.String
    object Boolean extends CalcUBType with CalcType.Boolean
    def apply(calcTypeRef : Calc)(implicit unsupported : TypeSymbol) : CalcUBType = {
      calcTypeRef match {
        case (t: CalcType.Char) => Char
        case (t: CalcType.Int) => Int
        case (t: CalcType.Long) => Long
        case (t: CalcType.Float) => Float
        case (t: CalcType.Double) => Double
        case (t: CalcType.String) => String
        case (t: CalcType.Boolean) => Boolean
        case _ => abort("Unsupported type")
      }
    }
  }

  sealed trait CalcNLit extends CalcVal
  object CalcNLit {
    implicit val lift = Liftable[CalcNLit] { p => p.tree }
    case class Char(override val tree : Tree, override val tpe : Type = CalcType.Char.tpe) extends CalcVal.Char('\u0001', tree) with CalcNLit
    case class Int(override val tree : Tree, override val tpe : Type = CalcType.Int.tpe) extends CalcVal.Int(1, tree) with CalcNLit
    case class Long(override val tree : Tree, override val tpe : Type = CalcType.Long.tpe) extends CalcVal.Long(1L, tree) with CalcNLit
    case class Float(override val tree : Tree, override val tpe : Type = CalcType.Float.tpe) extends CalcVal.Float(1.0f, tree) with CalcNLit
    case class Double(override val tree : Tree, override val tpe : Type = CalcType.Double.tpe) extends CalcVal.Double(1.0, tree) with CalcNLit
    case class String(override val tree : Tree, override val tpe : Type = CalcType.String.tpe) extends CalcVal.String("1", tree) with CalcNLit
    case class Boolean(override val tree : Tree, override val tpe : Type = CalcType.Boolean.tpe) extends CalcVal.Boolean(true, tree) with CalcNLit

    def apply[T](valueRef : T, tree : Tree)(implicit unsupported : TypeSymbol) : CalcNLit =
      CalcNLit(CalcLit(valueRef), tree)
    def apply(calcTypeRef : Calc, tree : Tree)(implicit unsupported : TypeSymbol) : CalcNLit = {
      calcTypeRef match {
        case (t : CalcType.Char) => Char(tree)
        case (t : CalcType.Int) => Int(tree)
        case (t : CalcType.Long) => Long(tree)
        case (t : CalcType.Float) => Float(tree)
        case (t : CalcType.Double) => Double(tree)
        case (t : CalcType.String) => String(tree)
        case (t : CalcType.Boolean) => Boolean(tree)
        case _ => abort("Unsupported type")
      }
    }
    def applyTpe(calcTypeRef : Calc, tree : Tree, tpe : Type)(implicit unsupported : TypeSymbol) : CalcNLit = {
      calcTypeRef match {
        case (t: CalcType.Char) => Char(tree, tpe)
        case (t: CalcType.Int) => Int(tree, tpe)
        case (t: CalcType.Long) => Long(tree, tpe)
        case (t: CalcType.Float) => Float(tree, tpe)
        case (t: CalcType.Double) => Double(tree, tpe)
        case (t: CalcType.String) => String(tree, tpe)
        case (t: CalcType.Boolean) => Boolean(tree, tpe)
        case _ => abort("Unsupported type")
      }
    }
    def unapply(arg: CalcNLit) : Option[Tree] = Some(arg.tree)
  }
  case class CalcUnknown(t: Type) extends Calc {
    val tpe = t
    val name = "Unknown"
  }
  object NonLiteralCalc {
    def unapply(tpe: Type): Option[CalcType] = tpe match {
      case TypeRef(_, sym, _) => sym match {
        case t if t == symbolOf[Char] => Some(CalcType.Char)
        case t if t == symbolOf[Int] => Some(CalcType.Int)
        case t if t == symbolOf[Long] => Some(CalcType.Long)
        case t if t == symbolOf[Float] => Some(CalcType.Float)
        case t if t == symbolOf[Double] => Some(CalcType.Double)
        case t if t == symbolOf[java.lang.String] => Some(CalcType.String)
        case t if t == symbolOf[Boolean] => Some(CalcType.Boolean)
        case t if t == symbolOf[scala.Symbol] => Some(CalcType.Symbol)
        case _ => None
      }
      case _ => None
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
    private def indentStr : String =  List.fill(indent * indentSize)(' ').mkString
    def incIdent : Unit = indent = indent + 1
    def decIdent : Unit = indent = indent - 1
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
      def unapply(tp: Type)(implicit annotatedSym : TypeSymbol): Option[CalcLit.Int] = {
        tp match {
          case TypeRef(_, sym, args) if sym == symbolOf[shapeless.Succ[_]] =>
            args.head match {
              case NatCalc(t) => Some(CalcLit.Int(t.value+1))
              case _ => abort(s"Given Nat type is defective: $tp, raw: ${showRaw(tp)}")
            }
          case TypeRef(_, sym, _) if sym == symbolOf[shapeless._0] =>
            Some(CalcLit.Int(0))
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
      def unapplyArg(calcTFType : Option[CalcTFType], tfArgType : Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
        TypeCalc.unapply(tfArgType) match {
          case Some(t : CalcLit) => Some(t)
          case _ => calcTFType
        }
      }

      def unapply(tp: Type)(implicit annotatedSym : TypeSymbol) : Option[Calc] = {
        val tfAnySym = symbolOf[TwoFaceAny[_,_]]
        tp match {
          case TypeRef(_, sym, args) if args.nonEmpty && tp.baseClasses.contains(tfAnySym) =>
            VerboseTraversal(s"@@TwoFaceCalc@@\nTP: $tp\nRAW: ${showRaw(tp)}\nBaseCls:${tp.baseClasses}")
            val calcTFType = sym match {
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Char[_]]) => Some(CalcTFType.Char)
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Int[_]]) => Some(CalcTFType.Int)
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Long[_]]) => Some(CalcTFType.Long)
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Float[_]]) => Some(CalcTFType.Float)
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Double[_]]) => Some(CalcTFType.Double)
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.String[_]]) => Some(CalcTFType.String)
              case t if tp.baseClasses.contains(symbolOf[TwoFaceAny.Boolean[_]]) => Some(CalcTFType.Boolean)
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
      def unapply(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
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
              case t if t == symbolOf[OpSymbol[_]] => Some(TypeCalc(args.head))
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
      def unapply(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
        tp match {
          case TypeRef(_, sym, args) if sym == symbolOf[OpMacro[_,_,_,_]] =>
            VerboseTraversal(s"@@OpCalc@@\nTP: $tp\nRAW: + ${showRaw(tp)}")
            val args = tp.typeArgs
            lazy val aValue = TypeCalc(args(1))
            lazy val bValue = TypeCalc(args(2))
            lazy val cValue = TypeCalc(args(3))
            val funcType = args.head.typeSymbol.asType

            if (funcType == funcTypes.GetType)
              return Some(CalcUnknown(args(1)))

            //If function is set/get variable we keep the original string,
            //otherwise we get the variable's value
            val retVal = (funcType, aValue) match {
              case (funcTypes.ImplicitFound, _) =>
                aValue match {
                  case CalcUnknown(t) => try {
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
                  case CalcUnknown(t) => Some(CalcLit(t.typeSymbol.asClass.knownDirectSubclasses.size))
                  case _ => Some(CalcLit(0))
                }
              case (funcTypes.IsNat, _) =>
                aValue match {
                  case CalcLit.Int(t) if t >= 0 => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsChar, _) =>
                aValue match {
                  case t : CalcType.Char => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsInt, _) =>
                aValue match {
                  case t : CalcType.Int => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsLong, _) =>
                aValue match {
                  case t : CalcType.Long => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsFloat, _) =>
                aValue match {
                  case t : CalcType.Float => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsDouble, _) =>
                aValue match {
                  case t : CalcType.Double => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsString, _) =>
                aValue match {
                  case t : CalcType.String => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsBoolean, _) =>
                aValue match {
                  case t : CalcType.Boolean => Some(CalcLit(true))
                  case _ => Some(CalcLit(false))
                }
              case (funcTypes.IsSymbol, _) =>
                aValue match {
                  case t : CalcType.String => Some(CalcLit(true))
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
                  case _ => None
                }
            }
            retVal
          case _ => None
        }
      }
    }
    ////////////////////////////////////////////////////////////////////////

    def apply(tp: Type)(implicit annotatedSym : TypeSymbol): Calc = {
      TypeCalc.unapply(tp) match {
        case Some(t : CalcVal) => t
        case Some(t : CalcType.Symbol) => CalcNLit(CalcType.String, q"valueOf[$tp].name")
        case Some(t : CalcUBType) => t
        case Some(t : CalcTFType) => CalcNLit(t, q"valueOf[$tp].getValue")
        case Some(t : CalcType) => CalcNLit(t, q"valueOf[$tp]")
        case Some(t : CalcUnknown) => t
        case _ =>
          VerboseTraversal(s"@@Unknown@@\nTP: $tp\nRAW: + ${showRaw(tp)}")
          CalcUnknown(tp)
      }
    }

    def unapply(tp: Type)(implicit annotatedSym : TypeSymbol): Option[Calc] = {
      val g = c.universe.asInstanceOf[SymbolTable]
      implicit def fixSymbolOps(sym: Symbol): g.Symbol = sym.asInstanceOf[g.Symbol]

      VerboseTraversal(s"${c.enclosingPosition}\nTP: $tp\nRAW: ${showRaw(tp)}")
      VerboseTraversal.incIdent
      val tpCalc = tp match {
        ////////////////////////////////////////////////////////////////////////
        // Value cases
        ////////////////////////////////////////////////////////////////////////
        case ConstantType(Constant(t)) => Some(CalcLit(t)) //Constant
        case OpCalc(t) => Some(t) // Operational Function
        case OpCastCalc(t) => Some(t) //Op Cast wrappers
        case TwoFaceCalc(t) => Some(t) //TwoFace values
        case NonLiteralCalc(t) => Some(t)// Non-literal values
        case NatCalc(t) => Some(t) //For Shapeless Nat
        case SingletonSymbolType(s) => Some(CalcLit(s)) //Symbol constant
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
          case Some(t) => Some(CalcUBType(t))
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
//          println("Exhausted search at: " + showRaw(tp))
          None
      }
      VerboseTraversal.decIdent
      tpCalc
    }
  }
  ////////////////////////////////////////////////////////////////////


  def abort(msg: String)(implicit annotatedSym : TypeSymbol): Nothing = {
    setAnnotation(msg)
    c.abort(c.enclosingPosition, msg)
  }

  def buildWarningMsgLoc : String = s"${c.enclosingPosition.source.path}:${c.enclosingPosition.line}:${c.enclosingPosition.column}"
  def buildWarningMsg(msg: String): String = s"Warning: $buildWarningMsgLoc    $msg"
  def buildWarningMsg(msg: Tree): Tree = q""" "Warning: " + $buildWarningMsgLoc + "    " + $msg """

  def constantTreeOf[T](t : T) : Tree = Literal(Constant(t))

  def constantTypeOf[T](t: T) : Type = c.internal.constantType(Constant(t))

  def genOpTreeLit[T](opTpe : Type, t: T)(implicit annotatedSym : TypeSymbol) : Tree = {
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

  def genOpTreeSymbol(opTpe : Type, t: String) : Tree = {
    val outTpe = SingletonSymbolType(t)
    val outTree = mkSingletonSymbol(t)
    val outWideTpe = typeOf[scala.Symbol]
    val outWideLiteral = mkSingletonSymbolWide(t)
    val outTypeName = TypeName("OutSymbol")
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

  def extractionFailed(tpe: Type)(implicit annotatedSym : TypeSymbol) = {
    val msg = s"Cannot extract value from $tpe\n" + "showRaw==> " + showRaw(tpe)
    abort(msg)
  }
  def extractionFailed(tree: Tree)(implicit annotatedSym : TypeSymbol) = {
    val msg = s"Cannot extract value from $tree\n" + "showRaw==> " + showRaw(tree)
    abort(msg)
  }

  def extractValueFromOpTree(opTree : c.Tree)(implicit annotatedSym : TypeSymbol) : CalcVal = {
    def outFindCond(elem : c.Tree) : Boolean = elem match {
      case q"final val value : $valueTpe = $valueTree" => true
      case _ => false
    }
    def getOut(opClsBlk : List[c.Tree]) : CalcVal = opClsBlk.find(outFindCond) match {
      case Some(q"final val value : $valueTpe = $valueTree") =>
        valueTree match {
          case Literal(Constant(t)) => CalcLit(t)
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

  def extractValueFromNumTree(numValueTree : c.Tree)(implicit annotatedSym : TypeSymbol) : CalcVal = {
    val typedTree = c.typecheck(numValueTree)
    TypeCalc(typedTree.tpe) match {
      case t : CalcLit => t
      case t : CalcUBType => CalcNLit.applyTpe(t, numValueTree, typedTree.tpe)
      case t : CalcType => CalcNLit(t, numValueTree)
      case _ => extractionFailed(typedTree.tpe)
    }
  }

  def extractValueFromTwoFaceTree(tfTree : c.Tree)(implicit annotatedSym : TypeSymbol) : CalcVal = {
    val typedTree = c.typecheck(tfTree)
    TypeCalc(typedTree.tpe) match {
      case t : CalcLit => t
      case t : CalcType => CalcNLit(t, q"$tfTree.getValue")
      case t =>
        println(t)
        extractionFailed(typedTree.tpe)
    }
  }

  def wideTypeName(tpe : Type) : String = tpe.widen.typeSymbol.name.toString

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Get Argument
  ///////////////////////////////////////////////////////////////////////////////////////////
  object MaterializeGetArg {
    def apply(gaSym : TypeSymbol, auxSym : TypeSymbol, aiTpe : Type, lhs : Boolean) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = auxSym
      val argIdx : Int = TypeCalc(aiTpe) match {
        case CalcLit.Int(t) => t
        case _ => abort(s"Invalid argument index. Found $aiTpe")
      }
      val valueTree : Tree = GetArgTree(argIdx, lhs)
      val outTpe : Type = c.typecheck(valueTree).tpe

      val genTree = q"""
        new $gaSym[$aiTpe]{
          type Out = $outTpe
          val value : Out = $valueTree
        }
     """
      //    println(genTree)
      genTree
    }
  }

  object GetArgTree {
    def isMethodMacroCall : Boolean = c.enclosingImplicits.last.sym.isMacro
    def getAllArgs(tree : Tree, lhs : Boolean) : List[Tree] = tree match {
      case Apply(Select(q,n), args) => if (isMethodMacroCall || lhs) args else List(tree)
      case t : Select => List(t)
      case t : Literal => List(t)
      case _ => getAllArgsRecur(tree)
    }

    def getAllArgsRecur(tree : Tree) : List[Tree] = tree match {
      case Apply(fun, args) => getAllArgsRecur(fun) ++ args
      case _ => List()
    }

    def getAllLHSArgs(tree : Tree)(implicit annotatedSym : TypeSymbol) : List[Tree] = tree match {
      case Apply(TypeApply(Select(t, _), _), _) => getAllArgs(t, true)
      case TypeApply(Select(t, _), _) => getAllArgs(t, true)
      case Select(t, _) => getAllArgs(t, true)
      case _ => abort("Left-hand-side tree not found")
    }

    def apply(argIdx : Int, lhs : Boolean)(implicit annotatedSym : TypeSymbol) : c.Tree = {
      val tree = c.enclosingImplicits.last.tree
//      println(">>>>>>> enclosingImpl: " + c.enclosingImplicits.last)
//      println("tree: " + c.enclosingImplicits.last.tree)
//      println("rawTree: " + showRaw(c.enclosingImplicits.last.tree))
      val allArgs = if (lhs) getAllLHSArgs(tree) else getAllArgs(tree, lhs)
//      println("args: " + allArgs)
//      println("<<<<<<< rawArgs" + showRaw(allArgs))
      if (argIdx < allArgs.length) allArgs(argIdx)
      else abort(s"Argument index($argIdx) is not smaller than the total number of arguments(${allArgs.length})")
    }
  }

  def extractFromArg(argIdx : Int, lhs : Boolean)(implicit annotatedSym : TypeSymbol) : CalcVal = {
    extractValueFromNumTree(GetArgTree(argIdx, lhs))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Three operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOpGen[F](implicit ev0: c.WeakTypeTag[F]): MaterializeOpAuxGen =
    new MaterializeOpAuxGen(weakTypeOf[F])

  def opCalc(funcType : TypeSymbol, aCalc : => Calc, bCalc : => Calc, cCalc : => Calc)(implicit annotatedSym : TypeSymbol) : Calc = {
    lazy val a = aCalc
    lazy val b = bCalc
    lazy val cArg = cCalc
    def unsupported() = {
      (a, b) match {
        case (aArg : CalcVal, bArg : CalcVal) => abort(s"Unsupported $funcType[$a, $b, $cArg]")
        case _ => CalcUnknown(funcType.toType)
      }
    }

    //The output val is literal if all arguments are literal. Otherwise, it is non-literal.
    lazy implicit val cvKind : CalcVal.Kind = (a, b, cArg) match {
      case (_ : CalcLit, _ : CalcLit, _ : CalcLit) => CalcVal.Lit
      case _ => CalcVal.NLit
    }

    def AcceptNonLiteral = Id //AcceptNonLiteral has a special handling in MaterializeOpAuxGen
    def GetArg = a match {
      case CalcLit.Int(t) if (t >= 0) => extractFromArg(t, false)
      case _ => unsupported()
    }
    def GetLHSArg = a match {
      case CalcLit.Int(t) if (t >= 0) => extractFromArg(t, true)
      case _ => unsupported()
    }
    def Id = a match {
      case (av : CalcVal) => av
      case _ => unsupported()
    }
    def ToNat = ToInt //Same handling, but also has a special case to handle this in MaterializeOpAuxGen
    def ToChar = a match {
      case CalcVal.Char(t, tt) => CalcVal(t, q"$tt")
      case CalcVal.Int(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal.Long(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal.Float(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case CalcVal.Double(t, tt) => CalcVal(t.toChar, q"$tt.toChar")
      case _ => unsupported()
    }
    def ToInt = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Int(t, tt) => CalcVal(t, q"$tt")
      case CalcVal.Long(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Float(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.Double(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case CalcVal.String(t, tt) => CalcVal(t.toInt, q"$tt.toInt")
      case _ => unsupported()
    }
    def ToLong = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.Int(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.Long(t, tt) => CalcVal(t, q"$tt")
      case CalcVal.Float(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.Double(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case CalcVal.String(t, tt) => CalcVal(t.toLong, q"$tt.toLong")
      case _ => unsupported()
    }
    def ToFloat = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.Int(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.Long(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.Float(t, tt) => CalcVal(t, q"$tt")
      case CalcVal.Double(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case CalcVal.String(t, tt) => CalcVal(t.toFloat, q"$tt.toFloat")
      case _ => unsupported()
    }
    def ToDouble = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Int(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Long(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Float(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case CalcVal.Double(t, tt) => CalcVal(t, q"$tt")
      case CalcVal.String(t, tt) => CalcVal(t.toDouble, q"$tt.toDouble")
      case _ => unsupported()
    }
    def ToString = a match {
      case CalcVal.Char(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Int(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Long(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Float(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.Double(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case CalcVal.String(t, tt) => CalcVal(t, q"$tt")
      case CalcVal.Boolean(t, tt) => CalcVal(t.toString, q"$tt.toString")
      case _ => unsupported()
    }
    def ToSymbol = ToString //Same handling, but has also has a special case in MaterializeOpAuxGen
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
      case CalcVal.Double(t, tt) => CalcVal(math.floor(t), q"_root_.scala.math.floor($tt)")
      case _ => unsupported()
    }
    def Ceil = a match {
      case CalcVal.Double(t, tt) => CalcVal(math.ceil(t), q"_root_.scala.math.ceil($tt)")
      case _ => unsupported()
    }
    def Round = a match {
      case CalcVal.Float(t, tt) => CalcVal(math.round(t), q"_root_.scala.math.round($tt)")
      case CalcVal.Double(t, tt) => CalcVal(math.round(t), q"_root_.scala.math.round($tt)")
      case _ => unsupported()
    }
    def Sin = a match {
      case CalcVal.Double(t, tt) => CalcVal(math.sin(t), q"_root_.scala.math.sin($tt)")
      case _ => unsupported()
    }
    def Cos = a match {
      case CalcVal.Double(t, tt) => CalcVal(math.cos(t), q"_root_.scala.math.cos($tt)")
      case _ => unsupported()
    }
    def Tan = a match {
      case CalcVal.Double(t, tt) => CalcVal(math.tan(t), q"_root_.scala.math.tan($tt)")
      case _ => unsupported()
    }
    def Sqrt = a match {
      case CalcVal.Double(t, tt) => CalcVal(math.sqrt(t), q"_root_.scala.math.sqrt($tt)")
      case _ => unsupported()
    }
    def Log = a match {
      case CalcVal.Double(t, tt) => CalcVal(math.log(t), q"_root_.scala.math.log($tt)")
      case _ => unsupported()
    }
    def Log10 = a match {
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
        case CalcLit.String(msg) => cArg match {
          case CalcUnknown(t) => if (t.typeSymbol == symbolOf[Warn]) {
            println(buildWarningMsg(msg))
            CalcLit(false)
          } else {
            //redirection of implicit not found annotation is required to the given symbol
            implicit val annotatedSym : TypeSymbol = t.typeSymbol.asType
            abort(msg)
          }
          case _ =>
            abort(msg)
        }
        //directly using the java lib `require` resulted in compiler crash, so we use wrapped require instead
        case CalcNLit.String(msg,_) => cArg match {
          case CalcUnknown(t) if t.typeSymbol == symbolOf[Warn] =>
            CalcNLit.Boolean(q"""{println(${buildWarningMsg(msg)}); false}""")
          case _ =>
            CalcNLit.Boolean(q"{_root_.singleton.ops.impl._require(false, $msg); false}")
        }
        case _ => unsupported()
      }
      case CalcNLit.Boolean(cond,_) => b match {
        //directly using the java lib `require` resulted in compiler crash, so we use wrapped require instead
        case CalcVal.String(msg, msgt) => cArg match {
          case CalcUnknown(t) if t == symbolOf[Warn] =>
            CalcNLit.Boolean(
              q"""{
                  if ($cond) true
                  else {
                    println(${buildWarningMsg(msgt)})
                    false
                  }
                }""")
          case _ =>
            CalcNLit.Boolean(q"{_root_.singleton.ops.impl._require($cond, $msgt); true}")
        }
        case _ => unsupported()
      }
      case _ => unsupported()
    }
    def ITE = (a, b, cArg) match {
      //Also has special case handling inside unapply
      case (CalcVal.Boolean(it,itt), CalcVal.Char(tt,ttt), CalcVal.Char(et,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal.Boolean(it,itt), CalcVal.Int(tt,ttt), CalcVal.Int(et,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal.Boolean(it,itt), CalcVal.Long(tt,ttt), CalcVal.Long(et,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal.Boolean(it,itt), CalcVal.Float(tt,ttt), CalcVal.Float(et,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal.Boolean(it,itt), CalcVal.Double(tt,ttt), CalcVal.Double(et,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal.Boolean(it,itt), CalcVal.String(tt,ttt), CalcVal.String(et,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case (CalcVal.Boolean(it,itt), CalcVal.Boolean(tt,ttt), CalcVal.Boolean(et,ett)) =>
        CalcVal(if(it) tt else et, q"if ($itt) $ttt else $ett")
      case _ => unsupported()
    }
    def Next = b match {
      case (bv : CalcVal) => bv
      case _ => unsupported()
    }
    def Plus = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case (CalcVal.String(at, att), CalcVal.String(bt, btt)) => CalcVal(at + bt, q"$att + $btt")
      case _ => unsupported()
    }
    def Minus = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at - bt, q"$att - $btt")
      case _ => unsupported()
    }
    def Mul = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at * bt, q"$att * $btt")
      case _ => unsupported()
    }
    def Div = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at / bt, q"$att / $btt")
      case _ => unsupported()
    }
    def Mod = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at % bt, q"$att % $btt")
      case _ => unsupported()
    }
    def Sml = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at < bt, q"$att < $btt")
      case _ => unsupported()
    }
    def Big = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at > bt, q"$att > $btt")
      case _ => unsupported()
    }
    def SmlEq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at <= bt, q"$att <= $btt")
      case _ => unsupported()
    }
    def BigEq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at >= bt, q"$att >= $btt")
      case _ => unsupported()
    }
    def Eq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.String(at, att), CalcVal.String(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at == bt, q"$att == $btt")
      case _ => unsupported()
    }
    def Neq = (a, b) match {
      case (CalcVal.Char(at, att), CalcVal.Char(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Int(at, att), CalcVal.Int(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Long(at, att), CalcVal.Long(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Float(at, att), CalcVal.Float(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Double(at, att), CalcVal.Double(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.String(at, att), CalcVal.String(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at != bt, q"$att != $btt")
      case _ => unsupported()
    }
    def And = a match {
      case CalcLit.Boolean(ab) => //`And` expressions where the LHS is a literal can be inlined
        if (ab) b match {
          case CalcVal.Boolean(_,_) => b //inlining the value of RHS when the LHS is true
          case _ => unsupported()
        } else CalcLit.Boolean(false) //inlining as false when the LHS is false
      case _ => (a, b) match {
        case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at && bt, q"$att && $btt")
        case _ => unsupported()
      }
    }
    def Or = a match {
      case CalcLit.Boolean(ab) => //`Or` expressions where the LHS is a literal can be inlined
        if (!ab) b match {
          case CalcVal.Boolean(_,_) => b //inlining the value of RHS when the LHS is false
          case _ => unsupported()
        } else CalcLit.Boolean(true) //inlining as true when the LHS is true
      case _ => (a, b) match {
        case (CalcVal.Boolean(at, att), CalcVal.Boolean(bt, btt)) => CalcVal(at || bt, q"$att || $btt")
        case _ => unsupported()
      }
    }
    def Pow = (a, b) match {
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
      case (CalcLit.String(at), CalcLit.Int(bt)) => CalcLit(at.substring(bt))
      case (CalcVal.String(at, att), CalcVal.Int(bt, btt)) => CalcNLit.String(q"$att.substring($btt)")
      case _ => unsupported()
    }
    def CharAt = (a, b) match {
      case (CalcLit.String(at), CalcLit.Int(bt)) => CalcLit(at.charAt(bt))
      case (CalcVal.String(at, att), CalcVal.Int(bt, btt)) => CalcNLit.Char(q"$att.charAt($btt)")
      case _ => unsupported()
    }
    def Length = a match {
      case CalcVal.String(at, att) => CalcVal(at.length, q"$att.length")
      case _ => unsupported()
    }

    funcType match {
      case funcTypes.AcceptNonLiteral => AcceptNonLiteral
      case funcTypes.GetArg => GetArg
      case funcTypes.GetLHSArg => GetLHSArg
      case funcTypes.Id => Id
      case funcTypes.ToNat => ToNat
      case funcTypes.ToChar => ToChar
      case funcTypes.ToInt => ToInt
      case funcTypes.ToLong => ToLong
      case funcTypes.ToFloat => ToFloat
      case funcTypes.ToDouble => ToDouble
      case funcTypes.ToString => ToString
      case funcTypes.ToSymbol => ToSymbol
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
      case _ => abort(s"Unsupported $funcType[$a, $b, $cArg]")
    }
  }

  final class MaterializeOpAuxGen(opTpe: Type) {
    def usingFuncName : Tree = {
      implicit val annotatedSym : TypeSymbol = symbolOf[OpMacro[_,_,_,_]]
      val funcType = opTpe.typeArgs.head.typeSymbol.asType
      val opResult = TypeCalc(opTpe)

      val genTree = (funcType, opResult) match {
        case (funcTypes.ToNat, CalcLit.Int(t)) =>
          if (t < 0) abort(s"Nat cannot be a negative literal. Found: $t")
          else genOpTreeNat(opTpe, t)
        case (funcTypes.ToSymbol, CalcLit.String(t)) => genOpTreeSymbol(opTpe, t)
        case (_, CalcLit(t)) => genOpTreeLit(opTpe, t)
        case (funcTypes.AcceptNonLiteral, t : CalcNLit) => genOpTreeNLit(opTpe, t)
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
      implicit val annotatedSym : TypeSymbol = shellAliasTpe
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
      implicit val annotatedSym : TypeSymbol = tfSym //not really used
//      println(tfSym.name)
      val genTree = genTwoFace(extractValueFromNumTree(numValueTree))
//      println(genTree)
      genTree
    }
    def toNumValue(tfTree : c.Tree, tfSym : TypeSymbol, tTpe : Type) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = tfSym
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
    def newChecked(calc : CalcVal, chkArgTpe : Type)(implicit annotatedSym : TypeSymbol) : c.Tree = {
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
        case (CalcLit.Boolean(true)) => CalcLit.String("") //Not calculating message if condition is constant true
        case _ => TypeCalc(fixedMsgTpe) match {
          case t : CalcVal => t
          case _ => extractionFailed(fixedMsgTpe)
        }
      }

      val reqCalc = opCalc(funcTypes.Require, condCalc, msgCalc, CalcLit(0))

      q"""
         (new $chkSym[$condTpe, $msgTpe, $chkArgTpe]($outTree.asInstanceOf[$outTpe]))
       """
    }
    def newChecked(calc : CalcVal)(implicit annotatedSym : TypeSymbol) : c.Tree = newChecked(calc, calc.tpe)
    def fromOpImpl(opTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = c.enclosingImplicits.head.pt match {
        case TypeRef(pre,sym,args) => sym.asType
        case _ => chkSym
      }
      val numValueCalc = extractValueFromOpTree(opTree)
      val genTree = newChecked(numValueCalc, tTpe)
//      println(genTree)
      genTree
    }
    def fromNumValue(numValueTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
      val numValueCalc = extractValueFromNumTree(numValueTree)
      val genTree = newChecked(numValueCalc)
//      println(genTree)
      genTree
    }
    def fromTF(tfTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
      val tfValueCalc = extractValueFromTwoFaceTree(tfTree)
      val genTree = newChecked(tfValueCalc)
//      println(genTree)
      genTree
    }
    def widen(chkTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
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
    def newChecked(tCalc : CalcVal, chkArgTpe : Type)(implicit annotatedSym : TypeSymbol) : c.Tree = {
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
        case (CalcLit.Boolean(true)) => CalcLit.String("") //Not calculating message if condition is constant true
        case _ => TypeCalc(fixedMsgTpe) match {
          case t : CalcVal => t
          case _ => extractionFailed(fixedMsgTpe)
        }
      }

      val reqCalc = opCalc(funcTypes.Require, condCalc, msgCalc, CalcLit(0))

      q"""
         (new $chkSym[$condTpe, $msgTpe, $chkArgTpe, $paramFaceTpe, $paramTpe]($outTree.asInstanceOf[$outTpe]))
       """
    }
    def newChecked(tCalc : CalcVal)(implicit annotatedSym : TypeSymbol) : c.Tree =
      newChecked(tCalc, tCalc.tpe)
    def fromOpImpl(tOpTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = c.enclosingImplicits.head.pt match {
        case TypeRef(pre,sym,args) => sym.asType
        case _ => chkSym
      }
      val tCalc = extractValueFromOpTree(tOpTree)
      val genTree = newChecked(tCalc, tTpe)
//      println(genTree)
      genTree
    }
    def fromNumValue(tNumTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
      val tCalc = extractValueFromNumTree(tNumTree)
      val genTree = newChecked(tCalc)
//      println(genTree)
      genTree
    }
    def fromTF(tTFTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
      val tCalc = extractValueFromTwoFaceTree(tTFTree)
      val genTree = newChecked(tCalc)
//      println(genTree)
      genTree
    }
    def widen(tTFTree : c.Tree) : c.Tree = {
      implicit val annotatedSym : TypeSymbol = chkSym
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


  //copied from Shapeless
  val SymTpe = typeOf[scala.Symbol]
  object SingletonSymbolType {
    val atatTpe = typeOf[@@[_,_]].typeConstructor
    val TaggedSym = typeOf[tag.Tagged[_]].typeConstructor.typeSymbol

    def unrefine(t: Type): Type =
      t.dealias match {
        case RefinedType(List(t), scope) if scope.isEmpty => unrefine(t)
        case t => t
      }

    def apply(s: String): Type = appliedType(atatTpe, List(SymTpe, c.internal.constantType(Constant(s))))

    def unapply(t: Type): Option[String] =
      unrefine(t).dealias match {
        case RefinedType(List(SymTpe, TypeRef(_, TaggedSym, List(ConstantType(Constant(s: String))))), _) => Some(s)
        case _ => None
      }
  }

  //copied from Shapeless
  def mkSingletonSymbol(s: String): Tree = {
    val sTpe = SingletonSymbolType(s)
    q"""_root_.scala.Symbol($s).asInstanceOf[$sTpe]"""
  }
  def mkSingletonSymbolWide(s: String): Tree = {
    q"""_root_.scala.Symbol($s)"""
  }
  ///////////////////////////////////////////////////////////////////////////////////////////
}
