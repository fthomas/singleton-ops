//package singleton.twoface
//
//import org.scalacheck.Properties
//import shapeless.test.illTyped
//import singleton.TestUtils._
//import singleton.ops._
//
//object CheckedIntSpec {
//
//  type Cond[T] = T < W.`50`.T
//  type Msg[T] = W.`"Failed Check"`.T
//
//  trait CheckedSmallerThan50Like extends Any with _root_.singleton.twoface.impl.Checked0Param[_CheckedSmallerThan50, Cond, Msg, Int] with _root_.singleton.twoface.impl.TwoFaceAny.IntLike {}
//
//  final class _CheckedSmallerThan50[T0](val value: Int) extends AnyVal with CheckedSmallerThan50Like {
//    type T = T0
//
//    @inline def getValue: Int = value
//  }
//
//  object CheckedSmallerThan50Like extends _root_.singleton.twoface.impl.Checked0Param.Builder[CheckedSmallerThan50, _CheckedSmallerThan50, Cond, Msg, Int] {
//    type Shell[T] = CheckedSmallerThan50Shell[T]
//  }
//
//  type CheckedSmallerThan50[T0] = CheckedSmallerThan50Like {type T <: T0}
//  val CheckedSmallerThan50 = CheckedSmallerThan50Like
//
//  final class CheckedSmallerThan50Shell[T] extends _root_.singleton.twoface.impl.Checked0ParamShell[CheckedSmallerThan50, Int, T] {
//    def apply(value: Int): CheckedSmallerThan50[T] = new _CheckedSmallerThan50[T](value)
//  }
//
//  object CheckedSmallerThan50Shell extends _root_.singleton.twoface.impl.Checked0ParamShell.Builder[CheckedSmallerThan50Shell, CheckedSmallerThan50, Cond, Msg, Int] {
//    def create[T]: CheckedSmallerThan50Shell[T] = new CheckedSmallerThan50Shell[T]
//  }
//
//}
//
//class CheckedIntSpec extends Properties("Checked.Int") {
//  import CheckedIntSpec._
//
//  def foo[T](t : TwoFace.Int[T]) : Unit = {}
//  def smallerThan50[T](t : CheckedSmallerThan50[T]) : Unit = {foo(t.unsafeCheck())}
//
//
//  property("Compile-time checks") = wellTyped {
//    CheckedSmallerThan50(5)
//    CheckedSmallerThan50[W.`5`.T]
//    CheckedSmallerThan50(TwoFace.Int(5))
//    val a = CheckedSmallerThan50[W.`5`.T + W.`3`.T]
//    implicitly[a.T <:< W.`8`.T]
//    implicitly[CheckedSmallerThan50[W.`5`.T]]
//    val b = implicitly[CheckedSmallerThan50[W.`5`.T + W.`3`.T]]
//    implicitly[b.T <:< (W.`5`.T + W.`3`.T)]
//    smallerThan50(40)
//    smallerThan50(TwoFace.Int(40))
//    smallerThan50(TwoFace.Int[W.`30`.T])
//    smallerThan50(implicitly[TwoFace.Int[W.`30`.T]])
//
//    illTyped("""CheckedSmallerThan50(50)""" ,"Failed Check")
//    illTyped("""CheckedSmallerThan50[W.`50`.T]""" ,"Failed Check")
//    illTyped("""CheckedSmallerThan50[W.`49`.T + W.`3`.T]""" ,"Failed Check")
//    illTyped("""smallerThan50(50)""" ,"Failed Check")
//    illTyped("""smallerThan50(TwoFace.Int(50))""", "Failed Check")
//  }
//
//  property("Run-time checks") = wellTyped {
//    smallerThan50(us(40))
//    smallerThan50(TwoFace.Int(us(40)))
//    illRun{smallerThan50(us(50))}
//    illRun{smallerThan50(TwoFace.Int(us(50)))}
//  }
//}
