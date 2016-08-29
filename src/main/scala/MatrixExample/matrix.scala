package MatrixExample

import singleton.ops._

trait Element {
  type E <: Element
  def + (that : E) : E
  def * (that : E) : E
  def * [R <: Int, C <: Int](that : Matrix[R, C, E]) : Matrix[R, C, E]
  def IAmElement() : Unit = println("I am Element")
}

class Matrix[R <: Int, C <: Int, E <: Element] {
  def + (that : Matrix[R, C, E]) : Matrix[R, C, E] = new Matrix[R, C, E]
  def * (that : E) : Matrix[R, C, E] = new Matrix[R, C, E]
  def * [C2 <: Int](that : Matrix[C, C2, E]) : Matrix[R, C2, E] = new Matrix[R, C2, E]
  def transpose : Matrix[C, R, E] = new Matrix[C, R, E]
}

class SquareMatrix[RC <: Int, E <: Element] extends Matrix[RC, RC, E] {
  def promoteBy[DeltaRC <: Int](implicit ev: Plus[Int, RC, DeltaRC]) : SquareMatrix[ev.Out, E] = new SquareMatrix[ev.Out, E]
}

class RVector[C <: Int, E <: Element] extends Matrix[1, C, E] {
  def IAmRVector() : Unit = println("I am Row Vector")
}

class CVector[R <: Int, E <: Element] extends Matrix[R, 1, E] {
  def IAmCVector() : Unit = println("I am Column Vector")
}

object Implicits {
  implicit def e2m[E <: Element](e : E) : Matrix[1, 1, E] = new Matrix[1, 1, E]
  implicit def m2sm[RC <: Int, E <: Element](m : Matrix[RC, RC, E]) : SquareMatrix[RC, E] = new SquareMatrix[RC, E]
  //implicit def m2e[E <: Element](m : Matrix[1, 1, E]) : E = ???
  implicit def m2r[C <: Int, E <: Element](m : Matrix[1, C, E]) = new RVector[C, E]
  implicit def m2c[R <: Int, E <: Element](m : Matrix[R, 1, E]) = new CVector[R, E]
}

case class MyElm(value : Int) extends Element {
  type E = MyElm
  def + (that : E) : E = MyElm(value)
  def * (that : E) : E = MyElm(value)
  def * [R <: Int, C <: Int](that : Matrix[R, C, E]) : Matrix[R, C, E] = new Matrix[R, C, E]
}
