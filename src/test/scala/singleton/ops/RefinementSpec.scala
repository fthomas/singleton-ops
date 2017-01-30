package singleton.ops

object RefinementSpec {
//  final val a  : Int with Singleton = 3
//  def demo[L <: Int with Singleton](implicit p : L + 0) : p.Out = p.value
//  val b : a.type = 3
  import impl._

  //  trait Op {
//    type Out <: Int with Singleton
//    val value : Out {}
//  }
//  def XTypeOf[S <: Int with Singleton](v : S) : Op {type Out = S} = new Op {type Out = S; val value : S {} = v}
  def id(a : Int with Singleton) = a

  final val in_works = 5
  val op_works = XTypeOf(in_works)
  val a_works : op_works.Out = op_works.value

  def test[ZeroOrOne <: XInt](x : ZeroOrOne)(implicit cond : Require[ZeroOrOne == 0]) : Unit = {}
  //test(5)
//  val a : 5 = op.value
}