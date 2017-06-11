package singleton.ops

import org.scalacheck.Prop

object TestUtils {
  def sameType[A, B](implicit ev: A =:= B): Boolean = true

  def wellTyped(body: => Unit): Prop = Prop.secure {
    body
    true
  }

  def illRun(body: => Unit) : Boolean = {
    val isIll = try {
      body
      false
    } catch {
      case _ : Throwable =>
        true
    }
    if (!isIll)
      assert(false, "Expected assertion did not occur")
    true
  }

  type Verify[OP, Result] = Require[(OP == Result) && (
        (IsNat[OP] && IsNat[Result]) ||
        (IsChar[OP] && IsChar[Result]) ||
        (IsInt[OP] && IsInt[Result]) ||
        (IsLong[OP] && IsLong[Result]) ||
        (IsFloat[OP] && IsFloat[Result]) ||
        (IsDouble[OP] && IsDouble[Result]) ||
        (IsString[OP] && IsString[Result]) ||
        (IsBoolean[OP] && IsBoolean[Result])
    )]

  def verifyOp[OP, Result](implicit verify: Verify[OP, Result]) : Prop = wellTyped {}
  def verifyOp1Args[OP[_],L,Result](implicit verify: Verify[OP[L], Result]) : Prop = wellTyped {}
  def verifyOp2Args[OP[_,_],L,R,Result](implicit verify: Verify[OP[L,R], Result]) : Prop = wellTyped {}
}
