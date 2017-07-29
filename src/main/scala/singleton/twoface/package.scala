//package singleton
//
//import singleton.ops.impl.OpId
//import twoface.impl._
//
//package object twoface {
//  import TwoFaceAny._
//
//  def ifThenElse(arg1: Boolean, arg2 : Char, arg3 : Char) : Char = macro Builder.Macro.triOp[OpId.ITE]
//  def ifThenElse(arg1: Boolean, arg2 : Int, arg3 : Int) : Int = macro Builder.Macro.triOp[OpId.ITE]
//  def ifThenElse(arg1: Boolean, arg2 : Long, arg3 : Long) : Long = macro Builder.Macro.triOp[OpId.ITE]
//  def ifThenElse(arg1: Boolean, arg2 : Float, arg3 : Float) : Float = macro Builder.Macro.triOp[OpId.ITE]
//  def ifThenElse(arg1: Boolean, arg2 : Double, arg3 : Double) : Double = macro Builder.Macro.triOp[OpId.ITE]
//  def ifThenElse(arg1: Boolean, arg2 : String, arg3 : String) : String = macro Builder.Macro.triOp[OpId.ITE]
//  def ifThenElse(arg1: Boolean, arg2 : Boolean, arg3 : Boolean) : Boolean = macro Builder.Macro.triOp[OpId.ITE]
//
//  object math {
//    import singleton.ops.math._
//    val Pi = new _Double[Pi](scala.math.Pi)
//    val E = new _Double[E](scala.math.E)
//
//    def abs(r : Int) : Int = macro Builder.Macro.unaryOp[OpId.Abs]
//    def abs(r : Long) : Long = macro Builder.Macro.unaryOp[OpId.Abs]
//    def abs(r : Float) : Float = macro Builder.Macro.unaryOp[OpId.Abs]
//    def abs(r : Double) : Double = macro Builder.Macro.unaryOp[OpId.Abs]
//
//    def min(l : Int, r : Int) : Int = macro Builder.Macro.binOp[OpId.Min]
//    def min(l : Long, r : Long) : Long = macro Builder.Macro.binOp[OpId.Min]
//    def min(l : Float, r : Float) : Float = macro Builder.Macro.binOp[OpId.Min]
//    def min(l : Double, r : Double) : Double = macro Builder.Macro.binOp[OpId.Min]
//
//    def max(l : Int, r : Int) : Int = macro Builder.Macro.binOp[OpId.Max]
//    def max(l : Long, r : Long) : Long = macro Builder.Macro.binOp[OpId.Max]
//    def max(l : Float, r : Float) : Float = macro Builder.Macro.binOp[OpId.Max]
//    def max(l : Double, r : Double) : Double = macro Builder.Macro.binOp[OpId.Max]
//
//    //    def pow[TX, TY](x : Float[TX], y : Float[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
////      tfo(scala.math.pow(x.getValue.toDouble, y.getValue.toDouble))
////    def pow[TX, TY](x : Float[TX], y : Double[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
////      tfo(scala.math.pow(x.getValue.toDouble, y.getValue))
////    def pow[TX, TY](x : Double[TX], y : Float[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
////      tfo(scala.math.pow(x.getValue, y.getValue.toDouble))
////    def pow[TX, TY](x : Double[TX], y : Double[TY])(implicit tfo : Double.Return[Pow[TX, TY]]) =
////      tfo(scala.math.pow(x.getValue, y.getValue))
////
////    def floor[T](x : Float[T])(implicit tfo : Double.Return[Floor[T]]) =
////      tfo(scala.math.floor(x.getValue.toDouble))
////    def floor[T](x : Double[T])(implicit tfo : Double.Return[Floor[T]]) =
////      tfo(scala.math.floor(x.getValue))
////
////    def ceil[T](x : Float[T])(implicit tfo : Double.Return[Ceil[T]]) =
////      tfo(scala.math.ceil(x.getValue.toDouble))
////    def ceil[T](x : Double[T])(implicit tfo : Double.Return[Ceil[T]]) =
////      tfo(scala.math.ceil(x.getValue))
////
////    def round[T](x : Float[T])(implicit tfo : Int.Return[Round[T]]) =
////      tfo(scala.math.round(x.getValue))
////    def round[T](x : Double[T])(implicit tfo : Long.Return[Round[T]]) =
////      tfo(scala.math.round(x.getValue))
////
////    def sin[T](x : Float[T])(implicit tfo : Double.Return[Sin[T]]) =
////      tfo(scala.math.sin(x.getValue.toDouble))
////    def sin[T](x : Double[T])(implicit tfo : Double.Return[Sin[T]]) =
////      tfo(scala.math.sin(x.getValue))
////
////    def cos[T](x : Float[T])(implicit tfo : Double.Return[Cos[T]]) =
////      tfo(scala.math.cos(x.getValue.toDouble))
////    def cos[T](x : Double[T])(implicit tfo : Double.Return[Cos[T]]) =
////      tfo(scala.math.cos(x.getValue))
////
////    def tan[T](x : Float[T])(implicit tfo : Double.Return[Tan[T]]) =
////      tfo(scala.math.tan(x.getValue.toDouble))
////    def tan[T](x : Double[T])(implicit tfo : Double.Return[Tan[T]]) =
////      tfo(scala.math.tan(x.getValue))
////
////    def sqrt[T](x : Float[T])(implicit tfo : Double.Return[Sqrt[T]]) =
////      tfo(scala.math.sqrt(x.getValue.toDouble))
////    def sqrt[T](x : Double[T])(implicit tfo : Double.Return[Sqrt[T]]) =
////      tfo(scala.math.sqrt(x.getValue))
////
////    def log[T](x : Float[T])(implicit tfo : Double.Return[Log[T]]) =
////      tfo(scala.math.log(x.getValue.toDouble))
////    def log[T](x : Double[T])(implicit tfo : Double.Return[Log[T]]) =
////      tfo(scala.math.log(x.getValue))
////
////    def log10[T](x : Float[T])(implicit tfo : Double.Return[Log10[T]]) =
////      tfo(scala.math.log10(x.getValue.toDouble))
////    def log10[T](x : Double[T])(implicit tfo : Double.Return[Log10[T]]) =
////      tfo(scala.math.log10(x.getValue))
//  }
//}
