package singleton.ops

object NewDemo {
  //////////////////////////////
  def demo[L <: Int with Singleton](implicit p : @@[@@[L]]) = mambo[p.OutInt]()
  def mambo[L <: Int with Singleton](){}
  val b = demo[8]
  //////////////////////////////

  //////////////////////////////
  def demoLong[L <: Long with Singleton](implicit p : @@[L]) = mamboLong[p.OutLong]()
  def mamboLong[L <: Long with Singleton](){}
  val bLong = demoLong[8L]
  //////////////////////////////

  //////////////////////////////
  def demoSumLongInt[L1 <: Long with Singleton, L2 <: Int with Singleton](implicit p : L1 + L2) : p.Out = p.value
  val bSumLongInt : 16L = demoSumLongInt[8L, 8]
  //////////////////////////////

  println("NewDemo " + b.toString)
}



class FixedSizeVector[L <: Int with Singleton]() {
  def concat[L2 <: Int with Singleton](that : FixedSizeVector[L2])(implicit l : L + L2) = new FixedSizeVector[l.OutInt]
  def + (that : FixedSizeVector[L]) = new FixedSizeVector[L]
}

object FixedSizeVector {
  def apply[L <: Int with Singleton](implicit check : Require[0 < L]) = new FixedSizeVector[L]
}

object TestVector {
  val v1 = FixedSizeVector[5]
  val v2 = FixedSizeVector[2]
  val v3 : FixedSizeVector[12] = v1 concat v2 concat v1
//  val v4 = FixedSizeVector[-1] Will leed to error
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