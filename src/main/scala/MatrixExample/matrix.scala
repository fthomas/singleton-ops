//package MatrixExample
//
//import singleton.ops._
//
//class Matrix[R <: XInt, C <: XInt] {
//  def + (that : Matrix[R, C]) : Matrix[R, C] = new Matrix[R, C]
//  def * (that : Double) : Matrix[R, C] = new Matrix[R, C]
//  def * [C2 <: XInt](that : Matrix[C, C2]) : Matrix[R, C2] = new Matrix[R, C2]
//  def transpose : Matrix[C, R] = new Matrix[C, R]
//}
//
//class SquareMatrix[RC <: XInt] extends Matrix[RC, RC] {
//  def promoteBy[DeltaRC <: XInt](implicit p: RC + DeltaRC) : SquareMatrix[p.OutInt] = new SquareMatrix[p.OutInt]
//
//}
//
//class RVector[C <: XInt] extends Matrix[1, C] {
//  def IAmRVector() : Unit = println("I am Row Vector")
//}
//
//class CVector[R <: XInt] extends Matrix[R, 1] {
//  def IAmCVector() : Unit = println("I am Column Vector")
//}
//
//object Implicits {
//  implicit def e2m(e : Double) : Matrix[1, 1] = new Matrix[1, 1]
//  implicit def m2sm[RC <: XInt](m : Matrix[RC, RC]) : SquareMatrix[RC] = new SquareMatrix[RC]
//  //implicit def m2e[Double <: Element](m : Matrix[1, 1]) : Double = ???
//  implicit def m2r[C <: XInt](m : Matrix[1, C]) = new RVector[C]
//  implicit def m2c[R <: XInt](m : Matrix[R, 1]) = new CVector[R]
//}
//
