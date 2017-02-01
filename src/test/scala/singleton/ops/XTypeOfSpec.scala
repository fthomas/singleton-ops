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
//  def demo[X] : X = valueOf2[X]

//  def bla[OP <: OpMacro[_ <: String with Singleton, _, _, _]]
//  (op: OP) : op.Out {} = op.value

  def imp[T](implicit t: T): t.type {} = t
  implicit def singletonToOp[X <: Singleton, N <: XString, S1, S2, S3, OP_OUT](x: X)
               (implicit v : ValueOf[X], op : OpMacro[N, S1, S2, S3], opaux: OpAux.Aux[OpMacro[N, S1, S2, S3], OP_OUT], check : Require[X == OP_OUT]) : OpMacro[N, S1, S2, S3] = op

  implicit def opToSingleton[N <: XString, S1, S2, S3, OP_OUT](op : OpMacro[N, S1, S2, S3])
               (implicit opaux: OpAux.Aux[OpMacro[N, S1, S2, S3], OP_OUT], v : ValueOf[OP_OUT]) : OP_OUT = valueOf[OP_OUT]

  implicit def opToOp[NA <: XString, SA1, SA2, SA3, NB <: XString, SB1, SB2, SB3, OP_OUTA, OP_OUTB](opA : OpMacro[NA, SA1, SA2, SA3])
               (implicit
                opauxA: OpAux.Aux[OpMacro[NA, SA1, SA2, SA3], OP_OUTA],
                opauxB: OpAux.Aux[OpMacro[NB, SB1, SB2, SB3], OP_OUTB],
                opB : OpMacro[NB, SB1, SB2, SB3],
                check : Require[OP_OUTA == OP_OUTB]) : OpMacro[NB, SB1, SB2, SB3] = opB


  val litToOp : 1 + 1 = 2
  val opToLit : 2 = valueOf[1 + 1]
  val opToOp : 4 + 1 = valueOf[2 + 3]



  trait Box[A]
  object Box {
    implicit def apply[A] : Box[A] = new Box[A]{}
  }
  implicit def blaBox[OP <: Op]
  (box: Box[OP])
  (implicit b: Box[2], check : ) : Box[2] = b

//  val a : 2 = bla[1+1]
//  val b2 = Box[1]
  val b1p1 : Box[2] = Box[1+1]
//  implicit def blaLong[N <: String with Singleton, S1, S2, S3]
//  (op : OpMacro[N, S1, S2, S3]) : Long = op.valueWide.asInstanceOf[Long]

//  val two : Int = demo[1L + 1 + 1]

//  val a : Int = valueOf[1 + 2]
}
