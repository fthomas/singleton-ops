//package singleton.ops
//
//object NewDemo {
//  //////////////////////////////
//  def demo[L <: XInt](implicit p : L*L + L) : p.Out{} = p.value
//  val b : 30 = demo[5]
//  //////////////////////////////
//
//  //////////////////////////////
//  import singleton.ops.math.Min
//  def demoLong[L1 <: XLong, L2 <: XLong](implicit p : Min[L1*L1, L2+L2]) : p.Out{} = p.value
//  val bLong1 : 1L = demoLong[1L, 5L]
//  val bLong2 : 6L = demoLong[3L, 3L]
//  //////////////////////////////
//
//  //////////////////////////////
//  def demoDouble[L1 <: XDouble, L2 <: XDouble](implicit p : L1 / L2 + 1.0) : p.Out{} = p.value
//  val bDouble : 1.2 = demoDouble[1.0, 5.0]
//  //////////////////////////////
//
//  //////////////////////////////
//  def demoSumLongInt[L1 <: XLong, L2 <: XInt](implicit p : L1 + L2) : p.Out{} = p.value
//  val bSumLongInt : 16L = demoSumLongInt[8L, 8]
//  //////////////////////////////
//
//  //////////////////////////////
//  def demoString[P1 <: XString](implicit op : Reverse[P1] + P1) : op.Out{} = op.value
//  val bString : "cbaabc" = demoString["abc"]
//  //////////////////////////////
//
//  //////////////////////////////
//  def demoBoolean[P1 <: XInt](implicit op : P1 < 0) : op.Out{} = op.value
//  val bBoolean1 : true = demoBoolean[-5]
//  val bBoolean2 : false = demoBoolean[5]
//  val bBoolean3 : false = demoBoolean[0]
//  //////////////////////////////
//
//  //////////////////////////////
//  def demoRequire[P1 <: XInt](implicit op : Require[P1 < 0]) : op.Out{} = op.value
//  demoRequire[-1]
//  //////////////////////////////
//
//  //////////////////////////////
//  import shapeless._
//  val n = Nat(5)
//  def demoNatToSingletonInt[L <: Nat](implicit p : L+L) : p.Out {} = p.value
//  val bSing10 : 10 = demoNatToSingletonInt[n.N]
//  def demoSigNatSig[L <: XInt](implicit op : SafeNat[L+L]) : op.Out = op.value
//  val bNat5 : shapeless.nat._10 = demoSigNatSig[5]
//  //////////////////////////////
//}
//
//object SimpleExample {
//  type PositiveInt[I] = Require[IsInt[I] && (I > 0)]
//
//  @scala.annotation.implicitNotFound(msg = "Evidence must be a positive integer")
//  abstract class FooEvidence[I](implicit require: PositiveInt[I]) {
//    def value: Int
//  }
//
//  implicit def valueOfFoo[I](implicit require: PositiveInt[I], plus3: SafeInt[I + 3]): FooEvidence[I] = new FooEvidence[I] {
//    def value: Int = plus3
//  }
//
//  val ev = implicitly[FooEvidence[4]]
//}
//
//
//object RightTriangleDemo {
//  type RightTriangle[A,B,C] =
//    ((A * A) + (B * B) == (C * C)) ||
//    ((A * A) + (C * C) == (B * B)) ||
//    ((B * B) + (C * C) == (A * A))
//
//  def fooWith90DegTriangle[A <: XDouble, B <: XDouble, C <: XDouble](implicit check : Require[RightTriangle[A,B,C]]) : Unit = {}
//  fooWith90DegTriangle[5.0,3.0,5.830951894845301] //OK!
//  shapeless.test.illTyped("fooWith90DegTriangle[5.0,3.1,5.830951894845301]")
//}
//
//
//object FixedSizedVectorDemo {
//  //In this method the length `L` is upper bounded by XInt
//  //In the `concat` method we must use `.OutInt` when applying the new type parameter `L + L2`
//  object Method_One {
//    type CheckPositive[I] = Require[I > 0]
//
//    class FixedSizeVector[L <: XInt] private () { //Here we chose to make the constructor private and
//                                                  //add constraints to the companion object.
//      def concat[L2 <: XInt](that : FixedSizeVector[L2])(implicit l : L + L2) = new FixedSizeVector[l.OutInt]
//      def + (that : FixedSizeVector[L]) = new FixedSizeVector[L]
//      def printLength(implicit length : SafeInt[L]) : Unit = println("Vector length is: " + length)
//    }
//
//    object FixedSizeVector {
//      def apply[L <: XInt](implicit check : CheckPositive[L]) = new FixedSizeVector[L]
//    }
//
//    object TestVector {
//      val v1 = FixedSizeVector[5]
//      val v2 = FixedSizeVector[2]
//      val v3 : FixedSizeVector[40] = v1 concat v2 concat v1 concat v2 concat v1 concat v2 concat v1 concat v2 concat v1 concat v2 concat v1
//      shapeless.test.illTyped("FixedSizeVector[-1]")
//    }
//  }
//
//  object Method_Two {
//    //In this method the length `L` is not upper bounded
//    //However the implicit constraint guards that this is a positive integer type.
//    //We gain something better with this method. We can input a type operation, and not just a type literal.
//    //E.g. FixedSizeVector[2 + 3].
//    //See the `concat` method in how it is helpful.
//    //We need to create equivalency between vectors of different inputs so we added an implicit keyword in
//    //the companion object.
//    //E.g. We want FixedSizeVector[2 + 3] to the same as FixedSizeVector[4 + 1] or FixedSizeVector[5]
//    //Note this will NOT be possible: implicitly[FixedSizeVector[2 + 3] =:= FixedSizeVector[5]]
//
//    type CheckPositive[I] = Require[IsInt[I] && (I > 0)]
//
//    class FixedSizeVector[L] private () { //Here we chose to make the constructor private and
//                                          //add constraints to the companion object.
//      def concat[L2](that : FixedSizeVector[L2]) = new FixedSizeVector[L + L2]
//      def + (that : FixedSizeVector[L]) = new FixedSizeVector[L]
//      def printLength(implicit length : SafeInt[L]) : Unit = println("Vector length is: " + length)
//    }
//
//    object FixedSizeVector {
//      implicit def apply[L](implicit check : CheckPositive[L]) = new FixedSizeVector[L]
//    }
//
//    object TestVector {
//      val v1 = FixedSizeVector[5]
//      val v2 = FixedSizeVector[2]
//      val v3 : FixedSizeVector[40] = v1 concat v2 concat v1 concat v2 concat v1 concat v2 concat v1 concat v2 concat v1 concat v2 concat v1
//      shapeless.test.illTyped("FixedSizeVector[-1]") //Will lead to error could not find implicit value for parameter check: singleton.ops.Require[singleton.ops.>[-1,0]]
//    }
//  }
//
//  object Method_Three { //Using TwoFace and Checked
//    import singleton.twoface._
//
//    object Test {
//      class FixedSizeVector[L] private (val length : TwoFace.Int[L]) {
//        def concat[L2](that : FixedSizeVector[L2])(implicit tfs : TwoFace.Int.Shell2[+,L,Int,L2,Int]) =
//          FixedSizeVector.protCreate(tfs(this.length,that.length))
//        def concat2[L2](that : FixedSizeVector[L2]) =
//          FixedSizeVector.protCreate(this.length + that.length)
//        override def toString = s"FixedSizeVector($length)"
//        def pretty(implicit rt: RunTime[L]) = if (rt) s"FixedSizeVector($length)" else s"FixedSizeVector[$length]"
//      }
//
//      object FixedSizeVector {
//        protected type CondCheckedLength[L] = L > 0
//        protected type MsgCheckedLength[L] = "Length must be positive (received value of " + ToString[L] + ")"
//        @checked0Param[CondCheckedLength, MsgCheckedLength, Int] class CheckedLength[L]
//
//        //Protected Constructor. No checks are required here.
//        protected def protCreate[L](tfLength : TwoFace.Int[L]) : FixedSizeVector[L] =
//          new FixedSizeVector[L](tfLength)
//
//        //Public Constructors. Perform compile-time check, if possible, and runtime, if not.
//        def apply[L](checkedLength : CheckedLength[L]) =
//          protCreate(checkedLength.unsafeCheck())
//        implicit def apply[L](implicit checkedLength : CheckedLength[L], di : DummyImplicit) =
//          protCreate(checkedLength.unsafeCheck())
//      }
//    }
//
//    object TestVector {
//      import Test._
//
//      //compile-time tests
//      val ctv5 : FixedSizeVector[5] = FixedSizeVector[5]
//      val ctv2 : FixedSizeVector[2] = FixedSizeVector(2)
//      val ctv7 : FixedSizeVector[7] = implicitly[FixedSizeVector[7]]
//      val ctv9 : FixedSizeVector[9] = ctv2 concat ctv7
//      val ctv9b : FixedSizeVector[9] = ctv2 concat2 ctv7
//      shapeless.test.illTyped("FixedSizeVector[0]")
//
//      //run-time tests
//      var two = 2
//      val rtv2 = FixedSizeVector(two)
//      val rtv4 = rtv2 concat rtv2 //runtime concat runtime => runtime
//      val rtv6 = rtv4 concat ctv2 //runtime concat compile-time => runtime
//      var zero = 0
//      FixedSizeVector(zero) //Run-time fail
//
//    }
//  }
//}
//
////object NonLiteralTest {
////  import singleton.twoface._
////
////  def smallerThan50(t : TwoFace.Int)
////                      (implicit r: Require[t.type < 50]) : Unit = {}
////
////  val forty = 40
////  val sixty = 60
////
//////  smallerThan50(forty) //passes run-time check
////  smallerThan50(40)    //passes compile-time check
//////  smallerThan50(sixty) //fails run-time check
//////  smallerThan50(60)    //fails compile-time check
////}
//
//
////
////object CheckedTest {
////  import singleton.twoface._
////
////  type CondSmallerThan50[T, P] = T < P
////  type MsgSmallerThan50[T, P] = "This is bad " + ToString[T]
////  type Param50 = 50
////  type SmallerThan50.Check[T] = Checked.Int[T, CondSmallerThan50, Param50, MsgSmallerThan50]
////  def smallerThan50[T](t : SmallerThan50.Check[T]) : Unit = {
////    require(t < 50, "") //if (rt_check)
////  }
////
////  var forty = 40
////  var sixty = 60
////  val tf40 = TwoFace.Int(40)
////  val tf60 = TwoFace.Int(60)
////  val tfForty = TwoFace.Int(forty)
////
////
////  smallerThan50(40)
////}
///* TODOs:
//Fix real world matrix example
//Add operations table to readme
//Add Log2 (efficient)
// */
//
