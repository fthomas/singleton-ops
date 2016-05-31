package singleton.ops

object TestUtils {
  def sameType[A, B](implicit ev: A =:= B): Boolean = true
}
