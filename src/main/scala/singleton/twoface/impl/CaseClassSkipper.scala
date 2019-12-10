package singleton.twoface.impl

import singleton.twoface.TwoFace

import scala.reflect.macros.whitebox

trait HasOut {
  type Out
}

sealed trait CaseClassSkipper[T <: HasOut] extends HasOut {
  type Out
  def apply(value : T => Any, fallBack : => Boolean) : Out
}
object CaseClassSkipper {
  type Aux[T <: HasOut, Out0] = CaseClassSkipper[T]{type Out = Out0}
  type TAux[T <: HasOut, Out0] = T {type Out = Out0}
  //Normal value retrieval
  implicit def evNormal[T <: HasOut](implicit t : T) : Aux[T, TwoFace.Boolean[t.Out]] = new CaseClassSkipper[T] {
    type Out = TwoFace.Boolean[t.Out]
    def apply(value: T => Any, fallBack: => Boolean): Out = value(t).asInstanceOf[Out]
  }
  //Fallback value retrieval
  case class Fail[T <: HasOut]() extends CaseClassSkipper[T] {
    type Out = Boolean
    def apply(value: T => Any, fallBack: => Boolean): Boolean = fallBack
  }
  implicit def evCC[T <: HasOut](implicit n : shapeless.Refute[T]) : Aux[T, Boolean] = macro evCCMacro[T]
  def evCCMacro[T <: HasOut](c: whitebox.Context)(n : c.Tree)(implicit wt : c.WeakTypeTag[T]) : c.Tree = {
    import c.universe._
    val t = weakTypeOf[T]
    if (c.internal.enclosingOwner.owner.asClass.isCaseClass)
      q"_root_.singleton.twoface.impl.CaseClassSkipper.Fail[$t]()"
    else
      c.abort(c.enclosingPosition, "Could not find implicit for...")
  }
}

