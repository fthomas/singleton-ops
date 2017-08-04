package singleton

import org.scalacheck.Prop
import singleton.ops._
import singleton.twoface._

object TestUtils {
  def sameType[A, B](implicit ev: A =:= B): Boolean = true

  def wellTyped(body: => Unit): Prop = Prop.secure {
    body
    true
  }

  def illRun(body: => Unit) : Prop = {
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

  type Verify[OP, Expected] = Require[(OP == Expected) && (
        (IsNat[OP] && IsNat[Expected]) ||
        (IsChar[OP] && IsChar[Expected]) ||
        (IsInt[OP] && IsInt[Expected]) ||
        (IsLong[OP] && IsLong[Expected]) ||
        (IsFloat[OP] && IsFloat[Expected]) ||
        (IsDouble[OP] && IsDouble[Expected]) ||
        (IsString[OP] && IsString[Expected]) ||
        (IsBoolean[OP] && IsBoolean[Expected])
    )]

  def verifyOp[OP, Expected](implicit verify: Verify[OP, Expected]) : Prop = wellTyped {}
  def verifyOp1Args[OP[_],L,Expected](implicit verify: Verify[OP[L], Expected]) : Prop = wellTyped {}
  def verifyOp2Args[OP[_,_],L,R,Expected](implicit verify: Verify[OP[L,R], Expected]) : Prop = wellTyped {}

  type VerifyTF[OP, Expected] = Require[ITE[IsNonLiteral[Expected], IsNonLiteral[OP], OP == Expected]]

  def verifyTFChar[OP, Expected](opResult : TwoFace.Char[OP], expectedResult : TwoFace.Char[Expected])
                            (implicit verify : VerifyTF[OP, Expected]) : Prop = {
    opResult.getValue == expectedResult.getValue
  }
  def verifyTFInt[OP, Expected](opResult : TwoFace.Int[OP], expectedResult : TwoFace.Int[Expected])
                               (implicit verify : VerifyTF[OP, Expected]) : Prop = {
    opResult.getValue == expectedResult.getValue
  }
  def verifyTFLong[OP, Expected](opResult : TwoFace.Long[OP], expectedResult : TwoFace.Long[Expected])
                               (implicit verify : VerifyTF[OP, Expected]) : Prop = {
    opResult.getValue == expectedResult.getValue
  }
  def verifyTFFloat[OP, Expected](opResult : TwoFace.Float[OP], expectedResult : TwoFace.Float[Expected])
                                (implicit verify : VerifyTF[OP, Expected]) : Prop = {
    opResult.getValue == expectedResult.getValue
  }
  def verifyTFDouble[OP, Expected](opResult : TwoFace.Double[OP], expectedResult : TwoFace.Double[Expected])
                            (implicit verify : VerifyTF[OP, Expected]) : Prop = {
    opResult.getValue == expectedResult.getValue
  }
  def verifyTFString[OP, Expected](opResult : TwoFace.String[OP], expectedResult : TwoFace.String[Expected])
                            (implicit verify : VerifyTF[OP, Expected]) : Prop = {
    opResult.getValue == expectedResult.getValue
  }
  def verifyTFBoolean[OP, Expected](opResult : TwoFace.Boolean[OP], expectedResult : TwoFace.Boolean[Expected])
                            (implicit verify : VerifyTF[OP, Expected]) : Prop = {
    opResult.getValue == expectedResult.getValue
  }

  //nf = unsafe. used to force a not-final value. e.g., nf(3) returns a non-literal 3
  def us[T](t : T) : T = {
    var ret = t
    ret
  }

  //Use to get the type of an object, when t.type is impossible
  trait Me {
    type T
  }
  def me[T0](t : T0) = new Me {
    type T = T0
  }
}
