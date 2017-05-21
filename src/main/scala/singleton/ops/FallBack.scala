package singleton.ops

/////////////////////////////////////////////////
//Fallback option for non-literals
/////////////////////////////////////////////////
trait FallBack[FB, OP] {
  type Out
  type Lit <: XBoolean
  val value : Option[FB]
  val isLiteral : Lit {}
}
object FallBack {
  type Aux[FB, OP, Ret_Out, Ret_Lit <: XBoolean] = FallBack[FB, OP]{type Out = Ret_Out; type Lit = Ret_Lit}
  implicit def evLiteral[FB, OP](implicit si : Id[OP]) : Aux[FB, OP, si.Out, true] =
  new FallBack[FB, OP] {
    type Out = si.Out
    type Lit = true
    val value : Option[FB] = Some(si.valueWide.asInstanceOf[FB])
    val isLiteral : Lit {} = true
  }
  implicit def evNotLiteral[FB, OP](implicit rq : Require[IsNotLiteral[OP]]) : Aux[FB, OP, FB, false] =
    new FallBack[FB, OP] {
      type Out = FB
      type Lit = false
      val value : Option[FB] = None
      val isLiteral : Lit {} = false
    }
}
/////////////////////////////////////////////////

