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
  implicit def evLiteral[FB, OP](implicit si : true ==> OP) : Aux[FB, OP, si.Out, true] =
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



final class MegaInt[I](val value : Int) extends AnyVal {
  def + [R : MegaInt](r : MegaInt[R])(implicit fb : FallBack[Int, I+R]) =
    MegaInt[fb.Out](if (fb.isLiteral) fb.value.get else this.value + r.value)
  def knownAtRunTime(implicit rt : RunTime[I]) : Boolean = rt
  def knownAtCompileTime(implicit rt : RunTime[I]) : Boolean = !rt
}

object MegaInt {
  def apply[I](i : Int) : MegaInt[I] = new MegaInt[I](i)
  implicit def safe[I <: Int with Singleton](i : I) : MegaInt[I] = new MegaInt[I](i)
  implicit def unsafe[I <: Int](i : I) : MegaInt[Int] = new MegaInt[Int](i)
  implicit def ev[I](implicit si : SafeInt[I]) : MegaInt[si.Out] = new MegaInt[si.Out](si)
}
