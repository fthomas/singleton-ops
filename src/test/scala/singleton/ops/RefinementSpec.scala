package singleton.ops

object RefinementSpec {
  final val a  : Int with Singleton = 3
  def demo[L <: Int with Singleton](implicit p : L + 0) : p.Out = p.value
  val b : a.type = 3

}