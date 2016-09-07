package singleton.ops


object NewDemo {
  import infixops._
  def mambo[L <: Int with Singleton](){}
  def demo[L <: Int with Singleton](implicit p : @@[L]) = mambo[p.Out]()
  def demo2[L <: Int with Singleton](implicit p : @@[L] + @@[L]) = mambo[p.Out]()
  def demo3[L <: Int with Singleton](implicit p : @@[L]) : p.Out {} = p.value
  val a = demo[8]
  val b : 8 = demo3[8]
  println("NewDemo " + b.toString)
}


//
//class FixedSizeVector[L <: Int with Singleton]() {
//  def concat[L2 <: Int with Singleton](that : FixedSizeVector[L2])(implicit l : @@[L] + @@[L2]) = new FixedSizeVector[l.Out]
//  def + (that : FixedSizeVector[L]) = new FixedSizeVector[L]
//}
//
//object FixedSizeVector {
//  def apply[L <: Int with Singleton](implicit check : LessThan[0, L]) = new FixedSizeVector[L]
//}
//
//object TestVector {
//  val v1 = FixedSizeVector[5]
//  val v2 = FixedSizeVector[2]
//  val v3 : FixedSizeVector[7] = v1 concat v2 //concat v1
////  val v4 = FixedSizeVector[-1]
//}


object TestMacro {
//  val a = SingletonTypeValueMacro.call[1.0]
}



//
//
//
//object Demo {
//  val a = Sum[1,2]
//  val av : a.Out = 3
//  val b = Sum[a.Out, 3]
//  val bv : b.Out = 6
//  def add3[A <: Int with Singleton, B <: Int with Singleton, C <: Int with Singleton, D <: Int with Singleton](implicit sum : Sum.Aux[Int, A, B, C], sum2 : Sum[Int, C, D]) = sum2
//
////  add3[1,2,0,3]
//  Sum[1,2]
//  Sum[1.0,2.0]
//}
//
//case class Foo[A <: Int with Singleton]() {
//  def foo[B <: Int with Singleton](implicit ev: Sum[Int, A, B]) = Foo[ev.Out]
//}