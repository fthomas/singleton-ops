package singleton.ops

/********************************************************************************************************
  * XTypeOf Experimental
  *******************************************************************************************************/
object XTypeOfSpec {
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
//    implicit def conv[N <: String with Singleton, S1, S2, S3]
//    (op : OpMacro[N, S1, S2, S3]) : 3 = 3
  def demo[X](implicit v : ValueOf[X]) : X = valueOf[X]
  val two : 2 = demo[1 + 1]
  val a : Int = valueOf[1 + 2]
}
