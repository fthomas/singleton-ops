package singleton.ops.impl


//This is just for testing the GetArg workaround a problem a string interpolator arguments are applied
protected[singleton] object InterpolatorTest {
  trait Bar
  trait Tag[W] extends HasOut {
    type Out = W
  }
  type XBar[W] = Bar with Tag[W]

  final implicit class InterpolatorSyntax(val sc: StringContext) {
    def bar(args: Bar*)(implicit interpolator : Interpolator[Bar]) : interpolator.Out = interpolator.value
  }
  trait Interpolator[T] extends HasOut {
    type Out <: T
    val value : Out
  }

  object Interpolator {
    type Aux[T, Out0 <: T] = Interpolator[T]{type Out = Out0}
    implicit def ev[W] : Interpolator.Aux[Bar, XBar[W]] = macro Macro.interpolator
  }

  protected object Macro {
    object whitebox { type Context = scala.reflect.macros.whitebox.Context }
    def interpolator(c: whitebox.Context) : c.Tree = {
      import c.universe._
      val widthTpe = c.internal.constantType(Constant(5))

      q"""
         new singleton.ops.impl.InterpolatorTest.Interpolator[singleton.ops.impl.InterpolatorTest.Bar] {
           type Out = singleton.ops.impl.InterpolatorTest.XBar[$widthTpe]
           val value : Out = new singleton.ops.impl.InterpolatorTest.Bar{}.asInstanceOf[singleton.ops.impl.InterpolatorTest.XBar[$widthTpe]]
         }
       """
    }
  }
}
