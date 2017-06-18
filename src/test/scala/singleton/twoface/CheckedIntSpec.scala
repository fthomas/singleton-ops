//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//class CheckedIntSpec extends Properties("Checked.Int") {
//  class FixedSizeVector[L] private (val length : TwoFace.Int[L]) {
//    def concat[L2](that : FixedSizeVector[L2]) = FixedSizeVector.protCreate(this.length + that.length)
//    override def toString = s"FixedSizeVector($length)"
//    def pretty(implicit rt: RunTime[L]) = if (rt) s"FixedSizeVector($length)" else s"FixedSizeVector[$length]"
//  }
//
//  object FixedSizeVector {
//    //Defining Checked Length Type
//    protected type CondCheckedLength[L, P] = L > 0
//    protected type ParamCheckedLength = 0
//    protected type MsgCheckedLength[L, P] = "Length must be positive (received value of " + ToString[L] + ")"
//    type CheckedLength[L] = Checked.Int[L, CondCheckedLength, ParamCheckedLength, MsgCheckedLength]
//
//    implicit object RuntimeCheckedLength extends Checked.Runtime[Int, Int, CondCheckedLength, MsgCheckedLength] {
//      def cond(l : Int, p : Option[Int]) : scala.Boolean = l > 0
//      def msg(l : Int, p : Option[Int]) : java.lang.String = s"Length must be positive (received value of $l)"
//    }
//
//    //Protected Constructor (performs unsafe run-time check, if compile-time check is not possible)
//    protected def protCreate[L](tfLength : TwoFace.Int[L]) : FixedSizeVector[L] =
//      new FixedSizeVector[L](tfLength)
//
//    //Public Constructors (perform compile-time check, if possible)
//    def apply[L](checkedLength : CheckedLength[L]) =
//      protCreate(checkedLength.unsafeCheck())
//    implicit def apply[L](implicit checkedLength : CheckedLength[L], di : DummyImplicit) =
//      protCreate(checkedLength.unsafeCheck())
//  }
//
//  property("Compile-time checks") = wellTyped {
//    val ctv5 : FixedSizeVector[5] = FixedSizeVector[5]
//    val ctv2 : FixedSizeVector[2] = FixedSizeVector(2)
//    val ctv7 : FixedSizeVector[7] = implicitly[FixedSizeVector[7]]
//    val ctv9 : FixedSizeVector[9] = ctv2 concat ctv7
//    illTyped("""FixedSizeVector(0)""")
//  }
//
//  property("Run-time checks") = wellTyped {
//    val ctv2 = FixedSizeVector(2)
//    val rtv2 = FixedSizeVector(us(2))
//    val rtv4 = rtv2 concat rtv2 //runtime concat runtime => runtime
//    val rtv6 = rtv4 concat ctv2 //runtime concat compile-time => runtime
//    illRun{FixedSizeVector(us(0))}
//  }
//}
