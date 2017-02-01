package singleton.ops.impl

import shapeless.Nat

trait Op extends Serializable {
  type Out
  type OutWide
  type OutNat <: Nat
  type OutChar <: Char with Singleton
  type OutInt <: Int with Singleton
  type OutLong <: Long with Singleton
  type OutFloat <: Float with Singleton
  type OutDouble <: Double with Singleton
  type OutString <: String with Singleton
  type OutBoolean <: Boolean with Singleton
  val value: Out {}
  val valueWide: OutWide
}

trait OpGen[O <: Op] {type Out; val value : Out}
object OpGen {
  type Aux[O <: Op, Ret_Out] = OpGen[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.Out] = new OpGen[O] {type Out = o.Out; val value = o.value}
}

trait OpNat[O <: Op] {type Out <: Nat; val value : Out}
object OpNat {
  type Aux[O <: Op, Ret_Out <: Nat] = OpNat[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutNat] = new OpNat[O] {type Out = o.OutNat; val value = o.value.asInstanceOf[o.OutNat]}
}

trait OpChar[O <: Op] {type Out <: Char with Singleton; val value : Out}
object OpChar {
  type Aux[O <: Op, Ret_Out <: Char with Singleton] = OpChar[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutChar] = new OpChar[O] {type Out = o.OutChar; val value = o.value.asInstanceOf[o.OutChar]}
}

trait OpInt[O <: Op] {type Out <: Int with Singleton; val value : Out}
object OpInt {
  type Aux[O <: Op, Ret_Out <: Int with Singleton] = OpInt[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutInt] = new OpInt[O] {type Out = o.OutInt; val value = o.value.asInstanceOf[o.OutInt]}
}

trait OpLong[O <: Op] {type Out <: Long with Singleton; val value : Out}
object OpLong {
  type Aux[O <: Op, Ret_Out <: Long with Singleton] = OpLong[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutLong] = new OpLong[O] {type Out = o.OutLong; val value = o.value.asInstanceOf[o.OutLong]}
}

trait OpFloat[O <: Op] {type Out <: Float with Singleton; val value : Out}
object OpFloat {
  type Aux[O <: Op, Ret_Out <: Float with Singleton] = OpFloat[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutFloat] = new OpFloat[O] {type Out = o.OutFloat; val value = o.value.asInstanceOf[o.OutFloat]}
}

trait OpDouble[O <: Op] {type Out <: Double with Singleton; val value : Out}
object OpDouble {
  type Aux[O <: Op, Ret_Out <: Double with Singleton] = OpDouble[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutDouble] = new OpDouble[O] {type Out = o.OutDouble; val value = o.value.asInstanceOf[o.OutDouble]}
}

trait OpString[O <: Op] {type Out <: String with Singleton; val value : Out}
object OpString {
  type Aux[O <: Op, Ret_Out <: String with Singleton] = OpString[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutString] = new OpString[O] {type Out = o.OutString; val value = o.value.asInstanceOf[o.OutString]}
}

trait OpBoolean[O <: Op] {type Out <: Boolean with Singleton; val value : Out}
object OpBoolean {
  type Aux[O <: Op, Ret_Out <: Boolean with Singleton] = OpBoolean[O] {type Out = Ret_Out}
  implicit def impl[O <: Op](implicit o: O) : Aux[O, o.OutBoolean] = new OpBoolean[O] {type Out = o.OutBoolean; val value = o.value.asInstanceOf[o.OutBoolean]}
}

