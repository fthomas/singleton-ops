package singleton.ops

//object Implicits {
//  def add[A <: Int with Singleton, B <: Int with Singleton](a: A, b: B)(implicit p : Plus[Int, A, B]) : p.Out {} = p.value
//  implicit class SingletonIntTypeClass[A <: Int with Singleton](a : A) {
//    def @+[B <: Int with Singleton](b : B)(implicit p : Plus[Int, A, B]) : p.Out {} = p.value
//  }
//  implicit class fsImpl[L <: Int with Singleton](f : FixedSizeVector[L]) {
//    def concat[L2 <: Int with Singleton](that : FixedSizeVector[L2]) = FixedSizeVector(f.length @+ that.length)
//  }
//}
//import Implicits._
case class FixedSizeVector[L <: Int with Singleton](length : L) {
  //def concat[L2 <: Int with Singleton](that : FixedSizeVector[L2]) = FixedSizeVector(length @+ that.length)
//  def concat[L2 <: Int with Singleton](that : FixedSizeVector[L2])(implicit p : Plus[Int, L, L2]) = FixedSizeVector(p.value)
  def + (that : FixedSizeVector[L]) = FixedSizeVector(length)
}


object MyFailedTest {
  val one : 1 = 1
  val two : 2 = 2
  def add[A <: Int with Singleton, B <: Int with Singleton](a: A, b: B)(implicit p : Plus[Int, A, B]) : p.Out {} = p.value
//  def add3[A <: Int with Singleton, B <: Int with Singleton, C <: Int with Singleton, X <: Int with Singleton](a: A, b: B, c: C)
//          (implicit p: Plus.Aux[Int, A, B, X], p2: Plus[Int, X, C]) : p2.Out {} = p2.value

//  def add3[A <: Int with Singleton, B <: Int with Singleton, C <: Int with Singleton, X](a: A, b: B, c: C)
//      (implicit p: Plus[Int, A, B], p2: Plus[Int, X, C]) : p2.Out {} = p2.value
//  val works : 3 = add3(1, 1, 1)
  val works2 : 2 = add(one, one)
  val works3 : 3 = add(two, one)
  val works4 : 5 = add(two, add(two, one))

  //val aa : 2 = add(CC(1).bad.value, CC(1).bad.value)
  //val a : FixedSizeVector[2] = FixedSizeVector(1) concat FixedSizeVector(1)
}
