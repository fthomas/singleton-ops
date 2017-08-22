package singleton.ops.impl

import shapeless.Nat

trait Op extends Serializable {
  type OutWide
  type Out
  type OutNat <: Nat
  type OutChar <: Char with Singleton
  type OutInt <: Int with Singleton
  type OutLong <: Long with Singleton
  type OutFloat <: Float with Singleton
  type OutDouble <: Double with Singleton
  type OutString <: String with Singleton
  type OutBoolean <: Boolean with Singleton
  type OutSymbol <: Symbol
  val value: Out
  val isLiteral : Boolean
  val valueWide: OutWide
}

protected[singleton] trait OpGen[O <: Op] {type Out; val value : Out}
protected[singleton] object OpGen {
  type Aux[O <: Op, Ret_Out] = OpGen[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.Out] = new OpGen[O] {type Out = o.Out; val value = o.value.asInstanceOf[o.Out]}
}

trait OpCast[T, O <: Op] {type Out <: T; val value : Out}


@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a Nat.")
trait OpNat[O <: Op] extends OpCast[Nat, O]
object OpNat {
  type Aux[O <: Op, Ret_Out <: Nat] = OpNat[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutNat] = new OpNat[O] {type Out = o.OutNat; val value = o.value.asInstanceOf[o.OutNat]}
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a Char.")
trait OpChar[O <: Op] extends OpCast[Char with Singleton, O]
object OpChar {
  type Aux[O <: Op, Ret_Out <: Char with Singleton] = OpChar[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutChar] = new OpChar[O] {type Out = o.OutChar; val value = o.value.asInstanceOf[o.OutChar]}
  implicit def conv[O <: Op](op : OpChar[O]) : Char = op.value
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is an Int.")
trait OpInt[O <: Op] extends OpCast[Int with Singleton, O]
object OpInt {
  type Aux[O <: Op, Ret_Out <: Int with Singleton] = OpInt[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutInt] = new OpInt[O] {type Out = o.OutInt; val value = o.value.asInstanceOf[o.OutInt]}
  implicit def conv[O <: Op](op : OpInt[O]) : Int = op.value
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a Long.")
trait OpLong[O <: Op] extends OpCast[Long with Singleton, O]
object OpLong {
  type Aux[O <: Op, Ret_Out <: Long with Singleton] = OpLong[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutLong] = new OpLong[O] {type Out = o.OutLong; val value = o.value.asInstanceOf[o.OutLong]}
  implicit def conv[O <: Op](op : OpLong[O]) : Long = op.value
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a Float.")
trait OpFloat[O <: Op] extends OpCast[Float with Singleton, O]
object OpFloat {
  type Aux[O <: Op, Ret_Out <: Float with Singleton] = OpFloat[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutFloat] = new OpFloat[O] {type Out = o.OutFloat; val value = o.value.asInstanceOf[o.OutFloat]}
  implicit def conv[O <: Op](op : OpFloat[O]) : Float = op.value
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a Double.")
trait OpDouble[O <: Op] extends OpCast[Double with Singleton, O]
object OpDouble {
  type Aux[O <: Op, Ret_Out <: Double with Singleton] = OpDouble[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutDouble] = new OpDouble[O] {type Out = o.OutDouble; val value = o.value.asInstanceOf[o.OutDouble]}
  implicit def conv[O <: Op](op : OpDouble[O]) : Double = op.value
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a String.")
trait OpString[O <: Op] extends OpCast[String with Singleton, O]
object OpString {
  type Aux[O <: Op, Ret_Out <: String with Singleton] = OpString[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutString] = new OpString[O] {type Out = o.OutString; val value = o.value.asInstanceOf[o.OutString]}
  implicit def conv[O <: Op](op : OpString[O]) : String = op.value
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a Boolean.")
trait OpBoolean[O <: Op] extends OpCast[Boolean with Singleton, O]
object OpBoolean {
  type Aux[O <: Op, Ret_Out <: Boolean with Singleton] = OpBoolean[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutBoolean] = new OpBoolean[O] {type Out = o.OutBoolean; val value = o.value.asInstanceOf[o.OutBoolean]}
  implicit def conv[O <: Op](op : OpBoolean[O]) : Boolean = op.value
}

@scala.annotation.implicitNotFound(msg = "Unable to prove type argument is a Symbol.")
trait OpSymbol[O <: Op] extends OpCast[Symbol, O]
object OpSymbol {
  type Aux[O <: Op, Ret_Out <: Symbol] = OpSymbol[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutSymbol] = new OpSymbol[O] {type Out = o.OutSymbol; val value = o.value.asInstanceOf[o.OutSymbol]}
  implicit def conv[O <: Op](op : OpSymbol[O]) : Symbol = op.value
}
