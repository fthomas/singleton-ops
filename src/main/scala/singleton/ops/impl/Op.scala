package singleton.ops.impl

import macrocompat.bundle
import shapeless.Nat

import scala.reflect.macros.whitebox
import singleton.ops.impl._

trait Op extends Serializable {
  type Out
  type OutWide
  type OutNat <: Nat
  type OutInt <: Int with Singleton
  type OutLong <: Long with Singleton
  type OutDouble <: Double with Singleton
  type OutString <: String with Singleton
  type OutBoolean <: Boolean with Singleton
  val value: Out {}
  val valueWide: OutWide
}
