package singleton.ops

object MyFailedTest {
  val one : 1 = 1
  def add[A <: Int with Singleton, B <: Int with Singleton](a: A, b: B)(implicit p : Plus[Int, A, B]) : p.Out {} = p.value
  val works = add(1, 1)
  val doesNotWork = add(one, one)
}
