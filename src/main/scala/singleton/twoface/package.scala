package singleton

import twoface.impl._

package object twoface {
  import TwoFaceAny._
  import singleton.ops._

  def abs[T](x : Int[T])(implicit tfo : Int.Return[Abs[T]]) = tfo(math.abs(x.getValue))
  def abs[T](x : Long[T])(implicit tfo : Long.Return[Abs[T]]) = tfo(math.abs(x.getValue))
  def abs[T](x : Float[T])(implicit tfo : Float.Return[Abs[T]]) = tfo(math.abs(x.getValue))
  def abs[T](x : Double[T])(implicit tfo : Double.Return[Abs[T]]) = tfo(math.abs(x.getValue))

  def pow[TX, TY](x : Float[TX], y : Float[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
    tfo(math.pow(x.getValue.toDouble, y.getValue.toDouble))
  def pow[TX, TY](x : Float[TX], y : Double[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
    tfo(math.pow(x.getValue.toDouble, y.getValue))
  def pow[TX, TY](x : Double[TX], y : Float[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
    tfo(math.pow(x.getValue, y.getValue.toDouble))
  def pow[TX, TY](x : Double[TX], y : Double[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
    tfo(math.pow(x.getValue, y.getValue))

  def floor[T](x : Float[T])(implicit tfo : Double.Return[Floor[T]]) =
    tfo(math.floor(x.getValue.toDouble))
  def floor[T](x : Double[T])(implicit tfo : Double.Return[Floor[T]]) =
    tfo(math.floor(x.getValue))

  def ceil[T](x : Float[T])(implicit tfo : Double.Return[Ceil[T]]) =
    tfo(math.ceil(x.getValue.toDouble))
  def ceil[T](x : Double[T])(implicit tfo : Double.Return[Ceil[T]]) =
    tfo(math.ceil(x.getValue))

  def round[T](x : Float[T])(implicit tfo : Int.Return[Round[T]]) =
    tfo(math.round(x.getValue))
  def round[T](x : Double[T])(implicit tfo : Long.Return[Round[T]]) =
    tfo(math.round(x.getValue))

  def sin[T](x : Float[T])(implicit tfo : Double.Return[Sin[T]]) =
    tfo(math.sin(x.getValue.toDouble))
  def sin[T](x : Double[T])(implicit tfo : Double.Return[Sin[T]]) =
    tfo(math.sin(x.getValue))

  def cos[T](x : Float[T])(implicit tfo : Double.Return[Cos[T]]) =
    tfo(math.cos(x.getValue.toDouble))
  def cos[T](x : Double[T])(implicit tfo : Double.Return[Cos[T]]) =
    tfo(math.cos(x.getValue))

  def tan[T](x : Float[T])(implicit tfo : Double.Return[Tan[T]]) =
    tfo(math.tan(x.getValue.toDouble))
  def tan[T](x : Double[T])(implicit tfo : Double.Return[Tan[T]]) =
    tfo(math.tan(x.getValue))

  def sqrt[T](x : Float[T])(implicit tfo : Double.Return[Sqrt[T]]) =
    tfo(math.sqrt(x.getValue.toDouble))
  def sqrt[T](x : Double[T])(implicit tfo : Double.Return[Sqrt[T]]) =
    tfo(math.sqrt(x.getValue))
}
