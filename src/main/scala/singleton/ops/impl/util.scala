package singleton.ops.impl

object nlz { //number of leading zeros
  def apply(value : Int) : Int = java.lang.Integer.numberOfLeadingZeros(value)
  def apply(value : Long) : Int = java.lang.Long.numberOfLeadingZeros(value)
}

object _require {
  @inline def apply(requirement: Boolean, message: => Any) = require(requirement, message)
}

object std {
  type Char = scala.Char
  type Int = scala.Int
  type Long = scala.Long
  type Float = scala.Float
  type Double = scala.Double
  type String = java.lang.String
  type Boolean = scala.Boolean
  type Symbol = scala.Symbol
}