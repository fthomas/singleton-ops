package singleton.ops


object NewDemo {
  import infixops._
  def demo[L <: Int with Singleton](implicit p : Sum2[@@[1], @@[1]]) : p.Out = p.value
  val a = demo
  println("NewDemo " + a.toString)
}














object Demo {
  val a = Sum[1,2]
  val av : a.Out = 3
  val b = Sum[a.Out, 3]
  val bv : b.Out = 6
  def add3[A <: Int with Singleton, B <: Int with Singleton, C <: Int with Singleton, D <: Int with Singleton](implicit sum : Sum.Aux[Int, A, B, C], sum2 : Sum[Int, C, D]) = sum2

//  add3[1,2,0,3]
  Sum[1,2]
  Sum[1.0,2.0]
}

case class Foo[A <: Int with Singleton]() {
  def foo[B <: Int with Singleton](implicit ev: Sum[Int, A, B]) = Foo[ev.Out]
}