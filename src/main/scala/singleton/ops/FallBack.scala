package singleton.ops

/////////////////////////////////////////////////
//Fallback option for non-literals
/////////////////////////////////////////////////
trait FallBack[Base, FB, OP] {
  type Out
  type Lit <: XBoolean
  val value : Option[Base]
  val isLiteral : Lit {}
}
object FallBack {
  type Aux[Base, FB, OP, Ret_Out, Ret_Lit <: XBoolean] = FallBack[Base, FB, OP]{type Out = Ret_Out; type Lit = Ret_Lit}
  implicit def evLiteral[Base, FB, OP](implicit si : Id[OP]) : Aux[Base, FB, OP, si.Out, true] =
  new FallBack[Base, FB, OP] {
    type Out = si.Out
    type Lit = true
    val value : Option[Base] = Some(si.valueWide.asInstanceOf[Base])
    val isLiteral : Lit {} = true
  }
  implicit def evNonLiteral[Base, FB, OP](implicit rq : Require[IsNonLiteral[OP]]) : Aux[Base, FB, OP, FB, false] =
    new FallBack[Base, FB, OP] {
      type Out = FB
      type Lit = false
      val value : Option[Base] = None
      val isLiteral : Lit {} = false
    }
}
/////////////////////////////////////////////////


///////////////////////////////////////////////////
////Fallback option for non-literals
///////////////////////////////////////////////////
//trait FallBack[Base, FB, OP] {
//  type Out
//  type Value
//  type Lit <: XBoolean
//  val value : Value
//  val isLiteral : Lit {}
//}
//object FallBack {
//  type Aux[Base, FB, OP, Ret_Out, Ret_Value, Ret_Lit <: XBoolean] =
//    FallBack[Base, FB, OP]{type Out = Ret_Out; type Value = Ret_Value; type Lit = Ret_Lit}
//  implicit def evKnown[Base, FB, OP](implicit si : AcceptNonLiteral[OP], lit : SafeBoolean[![IsNonLiteral[OP]]]) :
//  Aux[Base, FB, OP, si.Out, si.Out, lit.Out] =
//    new FallBack[Base, FB, OP] {
//      type Out = si.Out
//      type Value = si.Out
//      type Lit = lit.Out
//      val value : Value = si.value
//      val isLiteral : Lit {} = lit.value
//    }
//  implicit def evNonLiteral[Base, FB, OP](implicit rq : Require[IsUnknown[OP]]) :
//  Aux[Base, FB, OP, FB, false] =
//  new FallBack[Base, FB, OP] {
//    type Out = nl.Out
//    type Lit = false
//    val value : Option[Base] = None
//    val isLiteral : Lit {} = false
//  }
//}
///////////////////////////////////////////////////
//
