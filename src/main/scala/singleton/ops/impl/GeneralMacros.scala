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
    def unwrapVar(t : Constant) : Option[Constant] = {
      t match {
        case Constant(s : String) if Var.validName(s) =>
          val ret = Some(Var.get(s))
          ret
        case _ => Some(t)
      }

    }
    def unapplyVar(tp : Type) : Option[Constant] = {
      val maybeVar = unapply(tp)
      maybeVar match {
        case Some(t : Constant) => unwrapVar(t)
        case _ =>  maybeVar
      }
    }
    def unapply(tp: Type): Option[Constant] = {
//      print(tp + " RAW " + showRaw(tp))
      tp match {
        case tp @ ExistentialType(_, _) => unapply(tp.underlying)
        case TypeBounds(lo, hi) => unapply(hi)
        case RefinedType(parents, scope) =>
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

          //If function is set/get variable we keep the original string,
          //otherwise we get the variable's value
          val aValue = funcName match {
            case Some(Constant("SV")) => unapply(args(1))
            case Some(Constant("GV")) => unapply(args(1))
            case _ => unapplyVar(args(1))
          }
          val retVal = (funcName, aValue) match {
            case (Some(Constant("IsNotLiteral")), _) => Some(Constant(aValue.isEmpty)) //Looking for non literals (if returned empty)
            case (Some(Constant("ITE")), Some(Constant(cond : Boolean))) => //Special control case: ITE (If-Then-Else)
              if (cond)
                unapplyVar(args(2)) //true (then) part of the IF
              else
                unapplyVar(args(3)) //false (else) part of the IF
            case (Some(Constant("While")), Some(Constant(cond : Boolean))) => //Special control case: While
                var repeat = cond
                while (repeat) {
                  unapply(args(2)) //executing body
                  repeat = unapplyVar(args(1)) match { //reevaluate condition
                    case Some(Constant(updatedCond : Boolean)) => updatedCond
                    case _ => false
                  }
                }
              unapplyVar(args(3)) //return value of loop
            case _ => //regular cases
              val bValue = unapplyVar(args(2))
              val cValue = unapplyVar(args(3))
              (funcName, aValue, bValue, cValue) match {
                case (Some(Constant(f : String)), Some(Constant(a)), Some(Constant(b)), Some(Constant(c))) => Some(opCalc(f, a, b, c))
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

  def constantTypeAndValueOf[T](t: T): (Type, Literal, TypeName, Type, Tree) ={
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
      case _ => abort(s"Unsupported type $t", true)
    }
    (outWideType, outWideLiteral, outTypeName, constantTypeOf(t), Literal(Constant(t)))
  }

  def constantTypeAndValueOfNat(t: Int): (Type, Literal, TypeName, Type, Tree) ={
    val outWideType = typeOf[Int]
    val outWideLiteral = Literal(Constant(t))
    val outTypeName = TypeName("OutNat")
    (outWideType, outWideLiteral, outTypeName, mkNatTpe(t), q"new ${mkNatTpt(t)}")
  }

  def extractSingletonValue(tpe: Type): Constant = {
    def extractionFailed(tpe: Type, usePrint : Boolean) = {
      val msg = s"Cannot extract value from $tpe\n" + "showRaw==> " + showRaw(
        tpe)
      abort(msg, usePrint)
    }

    val value = tpe match {
      case Const(Constant(t)) => Constant(t)
      case _ => extractionFailed(tpe, false)
    }

    value
  }

  def evalTyped[T](expr: c.Expr[T]): T =
    c.eval(c.Expr[T](c.untypecheck(expr.tree)))

  object Var {
    val map = collection.mutable.Map[String, Constant]()
    def set(name : String, value : Constant) : Constant = {
      map(name) = value
      value
    }
    def get(name : String) : Constant = {
      map(name)
    }
    def validName(name : String) : Boolean = {
      name.startsWith("$")
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Three operands (Generic)
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOpGen[F, N](implicit ev0: c.WeakTypeTag[F], evn: c.WeakTypeTag[N]): MaterializeOpAuxGen =
    new MaterializeOpAuxGen(weakTypeOf[F], weakTypeOf[N])

  def opCalc[T1, T2, T3](funcName : String, aValue : T1, bValue : T2, cValue : T3) : Constant = {
    import scala.math._
    val ret = (funcName, aValue, bValue, cValue) match {
      case ("Id",         a: Char, _, _)              => Constant(a)
      case ("Id",         a: Int, _, _)               => Constant(a)
      case ("Id",         a: Long, _, _)              => Constant(a)
      case ("Id",         a: Float, _, _)             => Constant(a)
      case ("Id",         a: Double, _, _)            => Constant(a)
      case ("Id",         a: String, _, _)            => Constant(a)
      case ("Id",         a: Boolean, _, _)           => Constant(a)

      case ("ToNat",      a: Char, _, _)              => Constant(a.toInt)
      case ("ToNat",      a: Int, _, _)               => Constant(a.toInt)
      case ("ToNat",      a: Long, _, _)              => Constant(a.toInt)
      case ("ToNat",      a: Float, _, _)             => Constant(a.toInt)
      case ("ToNat",      a: Double, _, _)            => Constant(a.toInt)
      case ("ToNat",      a: String, _, _)            => Constant(a.toInt)

      case ("ToChar",     a: Char, _, _)              => Constant(a.toChar)
      case ("ToChar",     a: Int, _, _)               => Constant(a.toChar)
      case ("ToChar",     a: Long, _, _)              => Constant(a.toChar)
      case ("ToChar",     a: Float, _, _)             => Constant(a.toChar)
      case ("ToChar",     a: Double, _, _)            => Constant(a.toChar)

      case ("ToInt",      a: Char, _, _)              => Constant(a.toInt)
      case ("ToInt",      a: Int, _, _)               => Constant(a.toInt)
      case ("ToInt",      a: Long, _, _)              => Constant(a.toInt)
      case ("ToInt",      a: Float, _, _)             => Constant(a.toInt)
      case ("ToInt",      a: Double, _, _)            => Constant(a.toInt)
      case ("ToInt",      a: String, _, _)            => Constant(a.toInt)

      case ("ToLong",     a: Char, _, _)              => Constant(a.toLong)
      case ("ToLong",     a: Int, _, _)               => Constant(a.toLong)
      case ("ToLong",     a: Long, _, _)              => Constant(a.toLong)
      case ("ToLong",     a: Float, _, _)             => Constant(a.toLong)
      case ("ToLong",     a: Double, _, _)            => Constant(a.toLong)
      case ("ToLong",     a: String, _, _)            => Constant(a.toLong)

      case ("ToFloat",    a: Char, _, _)              => Constant(a.toFloat)
      case ("ToFloat",    a: Int, _, _)               => Constant(a.toFloat)
      case ("ToFloat",    a: Long, _, _)              => Constant(a.toFloat)
      case ("ToFloat",    a: Float, _, _)             => Constant(a.toFloat)
      case ("ToFloat",    a: Double, _, _)            => Constant(a.toFloat)
      case ("ToFloat",    a: String, _, _)            => Constant(a.toFloat)

      case ("ToDouble",   a: Char, _, _)              => Constant(a.toDouble)
      case ("ToDouble",   a: Int, _, _)               => Constant(a.toDouble)
      case ("ToDouble",   a: Long, _, _)              => Constant(a.toDouble)
      case ("ToDouble",   a: Float, _, _)             => Constant(a.toDouble)
      case ("ToDouble",   a: Double, _, _)            => Constant(a.toDouble)
      case ("ToDouble",   a: String, _, _)            => Constant(a.toDouble)

      case ("ToString",   a: Char, _, _)              => Constant(a.toString)
      case ("ToString",   a: Int, _, _)               => Constant(a.toString)
      case ("ToString",   a: Long, _, _)              => Constant(a.toString)
      case ("ToString",   a: Float, _, _)             => Constant(a.toString)
      case ("ToString",   a: Double, _, _)            => Constant(a.toString)
      case ("ToString",   a: String, _, _)            => Constant(a.toString)
      case ("ToString",   a: Boolean, _, _)           => Constant(a.toString)

      case ("IsNat",      a: Nat, _, _)               => Constant(true)
      case ("IsNat",      a: Char, _, _)              => Constant(false)
      case ("IsNat",      a: Int, _, _)               => Constant(false)
      case ("IsNat",      a: Long, _, _)              => Constant(false)
      case ("IsNat",      a: Float, _, _)             => Constant(false)
      case ("IsNat",      a: Double, _, _)            => Constant(false)
      case ("IsNat",      a: String, _, _)            => Constant(false)
      case ("IsNat",      a: Boolean, _, _)           => Constant(false)

      case ("IsChar",     a: Nat, _, _)               => Constant(false)
      case ("IsChar",     a: Char, _, _)              => Constant(true)
      case ("IsChar",     a: Int, _, _)               => Constant(false)
      case ("IsChar",     a: Long, _, _)              => Constant(false)
      case ("IsChar",     a: Float, _, _)             => Constant(false)
      case ("IsChar",     a: Double, _, _)            => Constant(false)
      case ("IsChar",     a: String, _, _)            => Constant(false)
      case ("IsChar",     a: Boolean, _, _)           => Constant(false)

      case ("IsInt",      a: Nat, _, _)               => Constant(false)
      case ("IsInt",      a: Char, _, _)              => Constant(false)
      case ("IsInt",      a: Int, _, _)               => Constant(true)
      case ("IsInt",      a: Long, _, _)              => Constant(false)
      case ("IsInt",      a: Float, _, _)             => Constant(false)
      case ("IsInt",      a: Double, _, _)            => Constant(false)
      case ("IsInt",      a: String, _, _)            => Constant(false)
      case ("IsInt",      a: Boolean, _, _)           => Constant(false)

      case ("IsLong",     a: Nat, _, _)               => Constant(false)
      case ("IsLong",     a: Char, _, _)              => Constant(false)
      case ("IsLong",     a: Int, _, _)               => Constant(false)
      case ("IsLong",     a: Long, _, _)              => Constant(true)
      case ("IsLong",     a: Float, _, _)             => Constant(false)
      case ("IsLong",     a: Double, _, _)            => Constant(false)
      case ("IsLong",     a: String, _, _)            => Constant(false)
      case ("IsLong",     a: Boolean, _, _)           => Constant(false)

      case ("IsFloat",    a: Nat, _, _)               => Constant(false)
      case ("IsFloat",    a: Char, _, _)              => Constant(false)
      case ("IsFloat",    a: Int, _, _)               => Constant(false)
      case ("IsFloat",    a: Long, _, _)              => Constant(false)
      case ("IsFloat",    a: Float, _, _)             => Constant(true)
      case ("IsFloat",    a: Double, _, _)            => Constant(false)
      case ("IsFloat",    a: String, _, _)            => Constant(false)
      case ("IsFloat",    a: Boolean, _, _)           => Constant(false)

      case ("IsDouble",   a: Nat, _, _)               => Constant(false)
      case ("IsDouble",   a: Char, _, _)              => Constant(false)
      case ("IsDouble",   a: Int, _, _)               => Constant(false)
      case ("IsDouble",   a: Long, _, _)              => Constant(false)
      case ("IsDouble",   a: Float, _, _)             => Constant(false)
      case ("IsDouble",   a: Double, _, _)            => Constant(true)
      case ("IsDouble",   a: String, _, _)            => Constant(false)
      case ("IsDouble",   a: Boolean, _, _)           => Constant(false)

      case ("IsString",   a: Nat, _, _)               => Constant(false)
      case ("IsString",   a: Char, _, _)              => Constant(false)
      case ("IsString",   a: Int, _, _)               => Constant(false)
      case ("IsString",   a: Long, _, _)              => Constant(false)
      case ("IsString",   a: Float, _, _)             => Constant(false)
      case ("IsString",   a: Double, _, _)            => Constant(false)
      case ("IsString",   a: String, _, _)            => Constant(true)
      case ("IsString",   a: Boolean, _, _)           => Constant(false)

      case ("IsBoolean",  a: Nat, _, _)               => Constant(false)
      case ("IsBoolean",  a: Char, _, _)              => Constant(false)
      case ("IsBoolean",  a: Int, _, _)               => Constant(false)
      case ("IsBoolean",  a: Long, _, _)              => Constant(false)
      case ("IsBoolean",  a: Float, _, _)             => Constant(false)
      case ("IsBoolean",  a: Double, _, _)            => Constant(false)
      case ("IsBoolean",  a: String, _, _)            => Constant(false)
      case ("IsBoolean",  a: Boolean, _, _)           => Constant(true)

      case ("Print",      a: Char, _, _)              => Constant(print(a.toString))
      case ("Print",      a: Int, _, _)               => Constant(print(a.toString))
      case ("Print",      a: Long, _, _)              => Constant(print(a.toString))
      case ("Print",      a: Float, _, _)             => Constant(print(a.toString))
      case ("Print",      a: Double, _, _)            => Constant(print(a.toString))
      case ("Print",      a: String, _, _)            => Constant(print(a.toString))
      case ("Print",      a: Boolean, _, _)           => Constant(print(a.toString))
        
      case ("Negate",     a: Char, _, _)              => Constant(-a)
      case ("Negate",     a: Int, _, _)               => Constant(-a)
      case ("Negate",     a: Long, _, _)              => Constant(-a)
      case ("Negate",     a: Float, _, _)             => Constant(-a)
      case ("Negate",     a: Double, _, _)            => Constant(-a)

      case ("Abs",        a: Int, _, _)               => Constant(abs(a))
      case ("Abs",        a: Long, _, _)              => Constant(abs(a))
      case ("Abs",        a: Float, _, _)             => Constant(abs(a))
      case ("Abs",        a: Double, _, _)            => Constant(abs(a))

      case ("Floor",      a: Float, _, _)             => Constant(floor(a.toDouble))
      case ("Floor",      a: Double, _, _)            => Constant(floor(a))

      case ("Ceil",       a: Float, _, _)             => Constant(ceil(a.toDouble))
      case ("Ceil",       a: Double, _, _)            => Constant(ceil(a))

      case ("Round",      a: Float, _, _)             => Constant(round(a))
      case ("Round",      a: Double, _, _)            => Constant(round(a))

      case ("Sin",        a: Float, _, _)             => Constant(sin(a.toDouble))
      case ("Sin",        a: Double, _, _)            => Constant(sin(a))

      case ("Cos",        a: Float, _, _)             => Constant(cos(a.toDouble))
      case ("Cos",        a: Double, _, _)            => Constant(cos(a))

      case ("Tan",        a: Float, _, _)             => Constant(tan(a.toDouble))
      case ("Tan",        a: Double, _, _)            => Constant(tan(a))

      case ("Sqrt",       a: Float, _, _)             => Constant(sqrt(a.toDouble))
      case ("Sqrt",       a: Double, _, _)            => Constant(sqrt(a))

      case ("Reverse",    a: String, _, _)            => Constant(a.reverse)
      case ("!",          a: Boolean, _, _)           => Constant(!a)
      case ("Require",    a: Boolean, _, _)           =>
        if (!a)
          abort(s"Cannot prove requirement Require[...]", false)
        else
          Constant(a)
      case ("==>",        _,          b,          _)  => Constant(b)
      case ("SV",         a: String,  b,          _)  => Var.set(a, Constant(b))
      case ("GV",         a: String,  _,          _)  => Var.get(a)

      case ("+",          a: Char,    b: Char,    _)  => Constant(a + b)
      case ("+",          a: Char,    b: Int,     _)  => Constant(a + b)
      case ("+",          a: Char,    b: Long,    _)  => Constant(a + b)
      case ("+",          a: Char,    b: Float,   _)  => Constant(a + b)
      case ("+",          a: Char,    b: Double,  _)  => Constant(a + b)
      case ("+",          a: Int,     b: Char,    _)  => Constant(a + b)
      case ("+",          a: Int,     b: Int,     _)  => Constant(a + b)
      case ("+",          a: Int,     b: Long,    _)  => Constant(a + b)
      case ("+",          a: Int,     b: Float,   _)  => Constant(a + b)
      case ("+",          a: Int,     b: Double,  _)  => Constant(a + b)
      case ("+",          a: Long,    b: Char,    _)  => Constant(a + b)
      case ("+",          a: Long,    b: Int,     _)  => Constant(a + b)
      case ("+",          a: Long,    b: Long,    _)  => Constant(a + b)
      case ("+",          a: Long,    b: Float,   _)  => Constant(a + b)
      case ("+",          a: Long,    b: Double,  _)  => Constant(a + b)
      case ("+",          a: Float,   b: Char,    _)  => Constant(a + b)
      case ("+",          a: Float,   b: Int,     _)  => Constant(a + b)
      case ("+",          a: Float,   b: Long,    _)  => Constant(a + b)
      case ("+",          a: Float,   b: Float,   _)  => Constant(a + b)
      case ("+",          a: Float,   b: Double,  _)  => Constant(a + b)
      case ("+",          a: Double,  b: Char,    _)  => Constant(a + b)
      case ("+",          a: Double,  b: Int,     _)  => Constant(a + b)
      case ("+",          a: Double,  b: Long,    _)  => Constant(a + b)
      case ("+",          a: Double,  b: Float,   _)  => Constant(a + b)
      case ("+",          a: Double,  b: Double,  _)  => Constant(a + b)
      case ("+",          a: String,  b: String,  _)  => Constant(a + b) //Concat

      case ("-",          a: Char,    b: Char,    _)  => Constant(a - b)
      case ("-",          a: Char,    b: Int,     _)  => Constant(a - b)
      case ("-",          a: Char,    b: Long,    _)  => Constant(a - b)
      case ("-",          a: Char,    b: Float,   _)  => Constant(a - b)
      case ("-",          a: Char,    b: Double,  _)  => Constant(a - b)
      case ("-",          a: Int,     b: Char,    _)  => Constant(a - b)
      case ("-",          a: Int,     b: Int,     _)  => Constant(a - b)
      case ("-",          a: Int,     b: Long,    _)  => Constant(a - b)
      case ("-",          a: Int,     b: Float,   _)  => Constant(a - b)
      case ("-",          a: Int,     b: Double,  _)  => Constant(a - b)
      case ("-",          a: Long,    b: Char,    _)  => Constant(a - b)
      case ("-",          a: Long,    b: Int,     _)  => Constant(a - b)
      case ("-",          a: Long,    b: Long,    _)  => Constant(a - b)
      case ("-",          a: Long,    b: Float,   _)  => Constant(a - b)
      case ("-",          a: Long,    b: Double,  _)  => Constant(a - b)
      case ("-",          a: Float,   b: Char,    _)  => Constant(a - b)
      case ("-",          a: Float,   b: Int,     _)  => Constant(a - b)
      case ("-",          a: Float,   b: Long,    _)  => Constant(a - b)
      case ("-",          a: Float,   b: Float,   _)  => Constant(a - b)
      case ("-",          a: Float,   b: Double,  _)  => Constant(a - b)
      case ("-",          a: Double,  b: Char,    _)  => Constant(a - b)
      case ("-",          a: Double,  b: Int,     _)  => Constant(a - b)
      case ("-",          a: Double,  b: Long,    _)  => Constant(a - b)
      case ("-",          a: Double,  b: Float,   _)  => Constant(a - b)
      case ("-",          a: Double,  b: Double,  _)  => Constant(a - b)

      case ("*",          a: Char,    b: Char,    _)  => Constant(a * b)
      case ("*",          a: Char,    b: Int,     _)  => Constant(a * b)
      case ("*",          a: Char,    b: Long,    _)  => Constant(a * b)
      case ("*",          a: Char,    b: Float,   _)  => Constant(a * b)
      case ("*",          a: Char,    b: Double,  _)  => Constant(a * b)
      case ("*",          a: Int,     b: Char,    _)  => Constant(a * b)
      case ("*",          a: Int,     b: Int,     _)  => Constant(a * b)
      case ("*",          a: Int,     b: Long,    _)  => Constant(a * b)
      case ("*",          a: Int,     b: Float,   _)  => Constant(a * b)
      case ("*",          a: Int,     b: Double,  _)  => Constant(a * b)
      case ("*",          a: Long,    b: Char,    _)  => Constant(a * b)
      case ("*",          a: Long,    b: Int,     _)  => Constant(a * b)
      case ("*",          a: Long,    b: Long,    _)  => Constant(a * b)
      case ("*",          a: Long,    b: Float,   _)  => Constant(a * b)
      case ("*",          a: Long,    b: Double,  _)  => Constant(a * b)
      case ("*",          a: Float,   b: Char,    _)  => Constant(a * b)
      case ("*",          a: Float,   b: Int,     _)  => Constant(a * b)
      case ("*",          a: Float,   b: Long,    _)  => Constant(a * b)
      case ("*",          a: Float,   b: Float,   _)  => Constant(a * b)
      case ("*",          a: Float,   b: Double,  _)  => Constant(a * b)
      case ("*",          a: Double,  b: Char,    _)  => Constant(a * b)
      case ("*",          a: Double,  b: Int,     _)  => Constant(a * b)
      case ("*",          a: Double,  b: Long,    _)  => Constant(a * b)
      case ("*",          a: Double,  b: Float,   _)  => Constant(a * b)
      case ("*",          a: Double,  b: Double,  _)  => Constant(a * b)

      case ("/",          a: Char,    b: Char,    _)  => Constant(a / b)
      case ("/",          a: Char,    b: Int,     _)  => Constant(a / b)
      case ("/",          a: Char,    b: Long,    _)  => Constant(a / b)
      case ("/",          a: Char,    b: Float,   _)  => Constant(a / b)
      case ("/",          a: Char,    b: Double,  _)  => Constant(a / b)
      case ("/",          a: Int,     b: Char,    _)  => Constant(a / b)
      case ("/",          a: Int,     b: Int,     _)  => Constant(a / b)
      case ("/",          a: Int,     b: Long,    _)  => Constant(a / b)
      case ("/",          a: Int,     b: Float,   _)  => Constant(a / b)
      case ("/",          a: Int,     b: Double,  _)  => Constant(a / b)
      case ("/",          a: Long,    b: Char,    _)  => Constant(a / b)
      case ("/",          a: Long,    b: Int,     _)  => Constant(a / b)
      case ("/",          a: Long,    b: Long,    _)  => Constant(a / b)
      case ("/",          a: Long,    b: Float,   _)  => Constant(a / b)
      case ("/",          a: Long,    b: Double,  _)  => Constant(a / b)
      case ("/",          a: Float,   b: Char,    _)  => Constant(a / b)
      case ("/",          a: Float,   b: Int,     _)  => Constant(a / b)
      case ("/",          a: Float,   b: Long,    _)  => Constant(a / b)
      case ("/",          a: Float,   b: Float,   _)  => Constant(a / b)
      case ("/",          a: Float,   b: Double,  _)  => Constant(a / b)
      case ("/",          a: Double,  b: Char,    _)  => Constant(a / b)
      case ("/",          a: Double,  b: Int,     _)  => Constant(a / b)
      case ("/",          a: Double,  b: Long,    _)  => Constant(a / b)
      case ("/",          a: Double,  b: Float,   _)  => Constant(a / b)
      case ("/",          a: Double,  b: Double,  _)  => Constant(a / b)

      case ("%",          a: Char,    b: Char,    _)  => Constant(a % b)
      case ("%",          a: Char,    b: Int,     _)  => Constant(a % b)
      case ("%",          a: Char,    b: Long,    _)  => Constant(a % b)
      case ("%",          a: Char,    b: Float,   _)  => Constant(a % b)
      case ("%",          a: Char,    b: Double,  _)  => Constant(a % b)
      case ("%",          a: Int,     b: Char,    _)  => Constant(a % b)
      case ("%",          a: Int,     b: Int,     _)  => Constant(a % b)
      case ("%",          a: Int,     b: Long,    _)  => Constant(a % b)
      case ("%",          a: Int,     b: Float,   _)  => Constant(a % b)
      case ("%",          a: Int,     b: Double,  _)  => Constant(a % b)
      case ("%",          a: Long,    b: Char,    _)  => Constant(a % b)
      case ("%",          a: Long,    b: Int,     _)  => Constant(a % b)
      case ("%",          a: Long,    b: Long,    _)  => Constant(a % b)
      case ("%",          a: Long,    b: Float,   _)  => Constant(a % b)
      case ("%",          a: Long,    b: Double,  _)  => Constant(a % b)
      case ("%",          a: Float,   b: Char,    _)  => Constant(a % b)
      case ("%",          a: Float,   b: Int,     _)  => Constant(a % b)
      case ("%",          a: Float,   b: Long,    _)  => Constant(a % b)
      case ("%",          a: Float,   b: Float,   _)  => Constant(a % b)
      case ("%",          a: Float,   b: Double,  _)  => Constant(a % b)
      case ("%",          a: Double,  b: Char,    _)  => Constant(a % b)
      case ("%",          a: Double,  b: Int,     _)  => Constant(a % b)
      case ("%",          a: Double,  b: Long,    _)  => Constant(a % b)
      case ("%",          a: Double,  b: Float,   _)  => Constant(a % b)
      case ("%",          a: Double,  b: Double,  _)  => Constant(a % b)

      case ("Pow",        a: Float,   b: Float,   _)  => Constant(pow(a.toDouble,b.toDouble))
      case ("Pow",        a: Float,   b: Double,  _)  => Constant(pow(a.toDouble,b.toDouble))
      case ("Pow",        a: Double,  b: Float,   _)  => Constant(pow(a.toDouble,b.toDouble))
      case ("Pow",        a: Double,  b: Double,  _)  => Constant(pow(a.toDouble,b.toDouble))

      case ("==",         a: Char,    b: Char,    _)  => Constant(a == b)
      case ("==",         a: Char,    b: Int,     _)  => Constant(a == b)
      case ("==",         a: Char,    b: Long,    _)  => Constant(a == b)
      case ("==",         a: Char,    b: Float,   _)  => Constant(a == b)
      case ("==",         a: Char,    b: Double,  _)  => Constant(a == b)
      case ("==",         a: Int,     b: Char,    _)  => Constant(a == b)
      case ("==",         a: Int,     b: Int,     _)  => Constant(a == b)
      case ("==",         a: Int,     b: Long,    _)  => Constant(a == b)
      case ("==",         a: Int,     b: Float,   _)  => Constant(a == b)
      case ("==",         a: Int,     b: Double,  _)  => Constant(a == b)
      case ("==",         a: Long,    b: Char,    _)  => Constant(a == b)
      case ("==",         a: Long,    b: Int,     _)  => Constant(a == b)
      case ("==",         a: Long,    b: Long,    _)  => Constant(a == b)
      case ("==",         a: Long,    b: Float,   _)  => Constant(a == b)
      case ("==",         a: Long,    b: Double,  _)  => Constant(a == b)
      case ("==",         a: Float,   b: Char,    _)  => Constant(a == b)
      case ("==",         a: Float,   b: Int,     _)  => Constant(a == b)
      case ("==",         a: Float,   b: Long,    _)  => Constant(a == b)
      case ("==",         a: Float,   b: Float,   _)  => Constant(a == b)
      case ("==",         a: Float,   b: Double,  _)  => Constant(a == b)
      case ("==",         a: Double,  b: Char,    _)  => Constant(a == b)
      case ("==",         a: Double,  b: Int,     _)  => Constant(a == b)
      case ("==",         a: Double,  b: Long,    _)  => Constant(a == b)
      case ("==",         a: Double,  b: Float,   _)  => Constant(a == b)
      case ("==",         a: Double,  b: Double,  _)  => Constant(a == b)
      case ("==",         a: String,  b: String,  _)  => Constant(a == b)
      case ("==",         a: Boolean, b: Boolean, _)  => Constant(a == b)

      case ("!=",         a: Char,    b: Char,    _)  => Constant(a != b)
      case ("!=",         a: Char,    b: Int,     _)  => Constant(a != b)
      case ("!=",         a: Char,    b: Long,    _)  => Constant(a != b)
      case ("!=",         a: Char,    b: Float,   _)  => Constant(a != b)
      case ("!=",         a: Char,    b: Double,  _)  => Constant(a != b)
      case ("!=",         a: Int,     b: Char,    _)  => Constant(a != b)
      case ("!=",         a: Int,     b: Int,     _)  => Constant(a != b)
      case ("!=",         a: Int,     b: Long,    _)  => Constant(a != b)
      case ("!=",         a: Int,     b: Float,   _)  => Constant(a != b)
      case ("!=",         a: Int,     b: Double,  _)  => Constant(a != b)
      case ("!=",         a: Long,    b: Char,    _)  => Constant(a != b)
      case ("!=",         a: Long,    b: Int,     _)  => Constant(a != b)
      case ("!=",         a: Long,    b: Long,    _)  => Constant(a != b)
      case ("!=",         a: Long,    b: Float,   _)  => Constant(a != b)
      case ("!=",         a: Long,    b: Double,  _)  => Constant(a != b)
      case ("!=",         a: Float,   b: Char,    _)  => Constant(a != b)
      case ("!=",         a: Float,   b: Int,     _)  => Constant(a != b)
      case ("!=",         a: Float,   b: Long,    _)  => Constant(a != b)
      case ("!=",         a: Float,   b: Float,   _)  => Constant(a != b)
      case ("!=",         a: Float,   b: Double,  _)  => Constant(a != b)
      case ("!=",         a: Double,  b: Char,    _)  => Constant(a != b)
      case ("!=",         a: Double,  b: Int,     _)  => Constant(a != b)
      case ("!=",         a: Double,  b: Long,    _)  => Constant(a != b)
      case ("!=",         a: Double,  b: Float,   _)  => Constant(a != b)
      case ("!=",         a: Double,  b: Double,  _)  => Constant(a != b)
      case ("!=",         a: String,  b: String,  _)  => Constant(a != b)
      case ("!=",         a: Boolean, b: Boolean, _)  => Constant(a != b)

      case ("<",          a: Char,    b: Char,    _)  => Constant(a < b)
      case ("<",          a: Char,    b: Int,     _)  => Constant(a < b)
      case ("<",          a: Char,    b: Long,    _)  => Constant(a < b)
      case ("<",          a: Char,    b: Float,   _)  => Constant(a < b)
      case ("<",          a: Char,    b: Double,  _)  => Constant(a < b)
      case ("<",          a: Int,     b: Char,    _)  => Constant(a < b)
      case ("<",          a: Int,     b: Int,     _)  => Constant(a < b)
      case ("<",          a: Int,     b: Long,    _)  => Constant(a < b)
      case ("<",          a: Int,     b: Float,   _)  => Constant(a < b)
      case ("<",          a: Int,     b: Double,  _)  => Constant(a < b)
      case ("<",          a: Long,    b: Char,    _)  => Constant(a < b)
      case ("<",          a: Long,    b: Int,     _)  => Constant(a < b)
      case ("<",          a: Long,    b: Long,    _)  => Constant(a < b)
      case ("<",          a: Long,    b: Float,   _)  => Constant(a < b)
      case ("<",          a: Long,    b: Double,  _)  => Constant(a < b)
      case ("<",          a: Float,   b: Char,    _)  => Constant(a < b)
      case ("<",          a: Float,   b: Int,     _)  => Constant(a < b)
      case ("<",          a: Float,   b: Long,    _)  => Constant(a < b)
      case ("<",          a: Float,   b: Float,   _)  => Constant(a < b)
      case ("<",          a: Float,   b: Double,  _)  => Constant(a < b)
      case ("<",          a: Double,  b: Char,    _)  => Constant(a < b)
      case ("<",          a: Double,  b: Int,     _)  => Constant(a < b)
      case ("<",          a: Double,  b: Long,    _)  => Constant(a < b)
      case ("<",          a: Double,  b: Float,   _)  => Constant(a < b)
      case ("<",          a: Double,  b: Double,  _)  => Constant(a < b)

      case (">",          a: Char,    b: Char,    _)  => Constant(a > b)
      case (">",          a: Char,    b: Int,     _)  => Constant(a > b)
      case (">",          a: Char,    b: Long,    _)  => Constant(a > b)
      case (">",          a: Char,    b: Float,   _)  => Constant(a > b)
      case (">",          a: Char,    b: Double,  _)  => Constant(a > b)
      case (">",          a: Int,     b: Char,    _)  => Constant(a > b)
      case (">",          a: Int,     b: Int,     _)  => Constant(a > b)
      case (">",          a: Int,     b: Long,    _)  => Constant(a > b)
      case (">",          a: Int,     b: Float,   _)  => Constant(a > b)
      case (">",          a: Int,     b: Double,  _)  => Constant(a > b)
      case (">",          a: Long,    b: Char,    _)  => Constant(a > b)
      case (">",          a: Long,    b: Int,     _)  => Constant(a > b)
      case (">",          a: Long,    b: Long,    _)  => Constant(a > b)
      case (">",          a: Long,    b: Float,   _)  => Constant(a > b)
      case (">",          a: Long,    b: Double,  _)  => Constant(a > b)
      case (">",          a: Float,   b: Char,    _)  => Constant(a > b)
      case (">",          a: Float,   b: Int,     _)  => Constant(a > b)
      case (">",          a: Float,   b: Long,    _)  => Constant(a > b)
      case (">",          a: Float,   b: Float,   _)  => Constant(a > b)
      case (">",          a: Float,   b: Double,  _)  => Constant(a > b)
      case (">",          a: Double,  b: Char,    _)  => Constant(a > b)
      case (">",          a: Double,  b: Int,     _)  => Constant(a > b)
      case (">",          a: Double,  b: Long,    _)  => Constant(a > b)
      case (">",          a: Double,  b: Float,   _)  => Constant(a > b)
      case (">",          a: Double,  b: Double,  _)  => Constant(a > b)

      case ("<=",         a: Char,    b: Char,    _)  => Constant(a <= b)
      case ("<=",         a: Char,    b: Int,     _)  => Constant(a <= b)
      case ("<=",         a: Char,    b: Long,    _)  => Constant(a <= b)
      case ("<=",         a: Char,    b: Float,   _)  => Constant(a <= b)
      case ("<=",         a: Char,    b: Double,  _)  => Constant(a <= b)
      case ("<=",         a: Int,     b: Char,    _)  => Constant(a <= b)
      case ("<=",         a: Int,     b: Int,     _)  => Constant(a <= b)
      case ("<=",         a: Int,     b: Long,    _)  => Constant(a <= b)
      case ("<=",         a: Int,     b: Float,   _)  => Constant(a <= b)
      case ("<=",         a: Int,     b: Double,  _)  => Constant(a <= b)
      case ("<=",         a: Long,    b: Char,    _)  => Constant(a <= b)
      case ("<=",         a: Long,    b: Int,     _)  => Constant(a <= b)
      case ("<=",         a: Long,    b: Long,    _)  => Constant(a <= b)
      case ("<=",         a: Long,    b: Float,   _)  => Constant(a <= b)
      case ("<=",         a: Long,    b: Double,  _)  => Constant(a <= b)
      case ("<=",         a: Float,   b: Char,    _)  => Constant(a <= b)
      case ("<=",         a: Float,   b: Int,     _)  => Constant(a <= b)
      case ("<=",         a: Float,   b: Long,    _)  => Constant(a <= b)
      case ("<=",         a: Float,   b: Float,   _)  => Constant(a <= b)
      case ("<=",         a: Float,   b: Double,  _)  => Constant(a <= b)
      case ("<=",         a: Double,  b: Char,    _)  => Constant(a <= b)
      case ("<=",         a: Double,  b: Int,     _)  => Constant(a <= b)
      case ("<=",         a: Double,  b: Long,    _)  => Constant(a <= b)
      case ("<=",         a: Double,  b: Float,   _)  => Constant(a <= b)
      case ("<=",         a: Double,  b: Double,  _)  => Constant(a <= b)

      case (">=",         a: Char,    b: Char,    _)  => Constant(a >= b)
      case (">=",         a: Char,    b: Int,     _)  => Constant(a >= b)
      case (">=",         a: Char,    b: Long,    _)  => Constant(a >= b)
      case (">=",         a: Char,    b: Float,   _)  => Constant(a >= b)
      case (">=",         a: Char,    b: Double,  _)  => Constant(a >= b)
      case (">=",         a: Int,     b: Char,    _)  => Constant(a >= b)
      case (">=",         a: Int,     b: Int,     _)  => Constant(a >= b)
      case (">=",         a: Int,     b: Long,    _)  => Constant(a >= b)
      case (">=",         a: Int,     b: Float,   _)  => Constant(a >= b)
      case (">=",         a: Int,     b: Double,  _)  => Constant(a >= b)
      case (">=",         a: Long,    b: Char,    _)  => Constant(a >= b)
      case (">=",         a: Long,    b: Int,     _)  => Constant(a >= b)
      case (">=",         a: Long,    b: Long,    _)  => Constant(a >= b)
      case (">=",         a: Long,    b: Float,   _)  => Constant(a >= b)
      case (">=",         a: Long,    b: Double,  _)  => Constant(a >= b)
      case (">=",         a: Float,   b: Char,    _)  => Constant(a >= b)
      case (">=",         a: Float,   b: Int,     _)  => Constant(a >= b)
      case (">=",         a: Float,   b: Long,    _)  => Constant(a >= b)
      case (">=",         a: Float,   b: Float,   _)  => Constant(a >= b)
      case (">=",         a: Float,   b: Double,  _)  => Constant(a >= b)
      case (">=",         a: Double,  b: Char,    _)  => Constant(a >= b)
      case (">=",         a: Double,  b: Int,     _)  => Constant(a >= b)
      case (">=",         a: Double,  b: Long,    _)  => Constant(a >= b)
      case (">=",         a: Double,  b: Float,   _)  => Constant(a >= b)
      case (">=",         a: Double,  b: Double,  _)  => Constant(a >= b)

      case ("&&",         a: Boolean, b: Boolean, _)  => Constant(a && b)
      case ("||",         a: Boolean, b: Boolean, _)  => Constant(a || b)

      case ("Min",        a: Int,     b: Int,     _)  => Constant(min(a, b))
      case ("Min",        a: Long,    b: Long,    _)  => Constant(min(a, b))
      case ("Min",        a: Float,   b: Float,   _)  => Constant(min(a, b))
      case ("Min",        a: Double,  b: Double,  _)  => Constant(min(a, b))

      case ("Max",        a: Int,     b: Int,     _)  => Constant(max(a, b))
      case ("Max",        a: Long,    b: Long,    _)  => Constant(max(a, b))
      case ("Max",        a: Float,   b: Float,   _)  => Constant(max(a, b))
      case ("Max",        a: Double,  b: Double,  _)  => Constant(max(a, b))

      case ("Substring",  a: String,  b: Int,     _)  => Constant(a.substring(b))
      case ("Length",     a: String,  _,          _)  => Constant(a.length)
      case ("CharAt",     a: String,  b: Int,     _)  => Constant(a.charAt(b))

      case _ => abort(s"Unsupported $funcName[$aValue, $bValue, $cValue]", true)
    }
    ret
  }

  final class MaterializeOpAuxGen(opTpe: Type, nTpe: Type) {
    def usingFuncName : Tree = {
      val funcName = extractSingletonValue(nTpe)
      val aValue = extractSingletonValue(opTpe)

      val (outWideTpe, outWideLiteral, outTypeName, outTpe, outTree) = (funcName, aValue) match {
        case (Constant("ToNat"), Constant(t : Int)) => constantTypeAndValueOfNat(t)
        case (_, Constant(t)) =>  constantTypeAndValueOf(t)
      }

      val genTree = q"""
        new $opTpe {
          type OutWide = $outWideTpe
          type Out = $outTpe {}
          type $outTypeName = $outTpe {}
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
  // Checked TwoFace
  ///////////////////////////////////////////////////////////////////////////////////////////
  def materializeOpVal[T]: MaterializeOpAuxVal[T] =
  new MaterializeOpAuxVal[T]

  final class MaterializeOpAuxVal[T] {
    def usingFuncName(value : c.Expr[T]) : c.Expr[T] = {
//      print(showRaw(condTpe))
//      print(showRaw(msgTpe))
//      print(showCode(value.tree))
//      print(showRaw(value.actualType))

      val genTree = value match {
        case (Expr(Literal(Constant(t)))) =>
          val outTpe = constantTypeOf(t)
          t match {
            case tt : Char => q"implicitly[_root_.singleton.twoface.Checked.Char[$outTpe]]"
            case tt : Int => q"implicitly[_root_.singleton.twoface.Checked.MyChecked[$outTpe]]"
            case tt : Long => q"implicitly[_root_.singleton.twoface.Checked.Long[$outTpe]]"
            case tt : Float => q"implicitly[_root_.singleton.twoface.Checked.Float[$outTpe]]"
            case tt : Double => q"implicitly[_root_.singleton.twoface.Checked.Double[$outTpe]]"
            case tt : String => q"implicitly[_root_.singleton.twoface.Checked.String[$outTpe]]"
            case tt : Boolean => q"implicitly[_root_.singleton.twoface.Checked.Boolean[$outTpe]]"
            case _ => abort(s"Unsupported type $t", true)
          }
        case _ =>
          val t = value.actualType
          val symName = t match {
            case (TypeRef(_, sym, _)) => sym.fullName
            case _ =>
              abort(s"Unsupported type $t", true)
          }
          symName match {
            case "scala.Char" => q"_root_.singleton.twoface.Checked.Char.create[scala.Char]($value)"
            case "scala.Int" => q"_root_.singleton.twoface.Checked.MyChecked.create[scala.Int]($value)"
            case "scala.Long" => q"_root_.singleton.twoface.Checked.Long.create[scala.Long]($value)"
            case "scala.Float" => q"_root_.singleton.twoface.Checked.Float.create[scala.Float]($value)"
            case "scala.Double" => q"_root_.singleton.twoface.Checked.Double.create[scala.Double]($value)"
            case "java.lang.String" => q"_root_.singleton.twoface.Checked.String.create[java.lang.String]($value)"
            case "scala.Boolean" => q"_root_.singleton.twoface.Checked.Boolean.create[scala.Boolean]($value)"
            case _ => abort(s"Unsupported type $t", true)
          }
      }

      c.Expr(genTree)
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
