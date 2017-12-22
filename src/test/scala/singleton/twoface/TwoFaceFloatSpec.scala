package singleton.twoface

import singleton.twoface.math._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class TwoFaceFloatSpec extends Properties("TwoFace.Float") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.Float[W.`2.0f`.T]]
    a.getValue == 2.0f && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.Float[W.`2.0f`.T]
    a.getValue == 2.0f && a.isLiteral
  }
  property("Safe Creation()") = {
    val a = TwoFace.Float(2.0f)
    a.getValue == 2.0f && a.isLiteral
  }
  property("Unsafe Creation()") = {
    val a = TwoFace.Float(us(2.0f))
    a.getValue == 2.0f && !a.isLiteral
  }

  property("Safe ifThenElse") = verifyTFFloat(ifThenElse(true, 1.0f, 2.0f), 1.0f)
  property("Unsafe ifThenElse") = verifyTFFloat(ifThenElse(us(false), 1.0f, 2.0f), us(2.0f))

  property("Safe Float + Safe Char") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Char('\u0001'), 3.0f)
  property("Safe Float + Unsafe Char") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Char(us('\u0001')), us(3.0f))
  property("Unsafe Float + Safe Char") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Char('\u0001'), us(3.0f))
  property("Unsafe Float + Unsafe Char") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Char(us('\u0001')), us(3.0f))
  property("Safe Float + Safe Int") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Int(1), 3.0f)
  property("Safe Float + Unsafe Int") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Int(us(1)), us(3.0f))
  property("Unsafe Float + Safe Int") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Int(1), us(3.0f))
  property("Unsafe Float + Unsafe Int") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Int(us(1)), us(3.0f))
  property("Safe Float + Safe Long") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Long(1L), 3.0f)
  property("Safe Float + Unsafe Long") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Long(us(1L)), us(3.0f))
  property("Unsafe Float + Safe Long") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Long(1L), us(3.0f))
  property("Unsafe Float + Unsafe Long") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Long(us(1L)), us(3.0f))
  property("Safe Float + Safe Float") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Float(1.0f), 3.0f)
  property("Safe Float + Unsafe Float") = verifyTFFloat(TwoFace.Float(2.0f) + TwoFace.Float(us(1.0f)), us(3.0f))
  property("Unsafe Float + Safe Float") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Float(1.0f), us(3.0f))
  property("Unsafe Float + Unsafe Float") = verifyTFFloat(TwoFace.Float(us(2.0f)) + TwoFace.Float(us(1.0f)), us(3.0f))
  property("Safe Float + Safe Double") = verifyTFDouble(TwoFace.Float(2.0f) + TwoFace.Double(1.0), 3.0)
  property("Safe Float + Unsafe Double") = verifyTFDouble(TwoFace.Float(2.0f) + TwoFace.Double(us(1.0)), us(3.0))
  property("Unsafe Float + Safe Double") = verifyTFDouble(TwoFace.Float(us(2.0f)) + TwoFace.Double(1.0), us(3.0))
  property("Unsafe Float + Unsafe Double") = verifyTFDouble(TwoFace.Float(us(2.0f)) + TwoFace.Double(us(1.0)), us(3.0))

  property("Safe Float - Safe Char") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Char('\u0001'), 1.0f)
  property("Safe Float - Unsafe Char") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Char(us('\u0001')), us(1.0f))
  property("Unsafe Float - Safe Char") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Char('\u0001'), us(1.0f))
  property("Unsafe Float - Unsafe Char") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Char(us('\u0001')), us(1.0f))
  property("Safe Float - Safe Int") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Int(1), 1.0f)
  property("Safe Float - Unsafe Int") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Int(us(1)), us(1.0f))
  property("Unsafe Float - Safe Int") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Int(1), us(1.0f))
  property("Unsafe Float - Unsafe Int") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Int(us(1)), us(1.0f))
  property("Safe Float - Safe Long") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Long(1L), 1.0f)
  property("Safe Float - Unsafe Long") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Long(us(1L)), us(1.0f))
  property("Unsafe Float - Safe Long") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Long(1L), us(1.0f))
  property("Unsafe Float - Unsafe Long") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Long(us(1L)), us(1.0f))
  property("Safe Float - Safe Float") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Float(1.0f), 1.0f)
  property("Safe Float - Unsafe Float") = verifyTFFloat(TwoFace.Float(2.0f) - TwoFace.Float(us(1.0f)), us(1.0f))
  property("Unsafe Float - Safe Float") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Float(1.0f), us(1.0f))
  property("Unsafe Float - Unsafe Float") = verifyTFFloat(TwoFace.Float(us(2.0f)) - TwoFace.Float(us(1.0f)), us(1.0f))
  property("Safe Float - Safe Double") = verifyTFDouble(TwoFace.Float(2.0f) - TwoFace.Double(1.0), 1.0)
  property("Safe Float - Unsafe Double") = verifyTFDouble(TwoFace.Float(2.0f) - TwoFace.Double(us(1.0)), us(1.0))
  property("Unsafe Float - Safe Double") = verifyTFDouble(TwoFace.Float(us(2.0f)) - TwoFace.Double(1.0), us(1.0))
  property("Unsafe Float - Unsafe Double") = verifyTFDouble(TwoFace.Float(us(2.0f)) - TwoFace.Double(us(1.0)), us(1.0))

  property("Safe Float * Safe Char") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Char('\u0001'), 2.0f)
  property("Safe Float * Unsafe Char") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Char(us('\u0001')), us(2.0f))
  property("Unsafe Float * Safe Char") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Char('\u0001'), us(2.0f))
  property("Unsafe Float * Unsafe Char") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Char(us('\u0001')), us(2.0f))
  property("Safe Float * Safe Int") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Int(1), 2.0f)
  property("Safe Float * Unsafe Int") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Int(us(1)), us(2.0f))
  property("Unsafe Float * Safe Int") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Int(1), us(2.0f))
  property("Unsafe Float * Unsafe Int") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Int(us(1)), us(2.0f))
  property("Safe Float * Safe Long") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Long(1L), 2.0f)
  property("Safe Float * Unsafe Long") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Long(us(1L)), us(2.0f))
  property("Unsafe Float * Safe Long") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Long(1L), us(2.0f))
  property("Unsafe Float * Unsafe Long") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Long(us(1L)), us(2.0f))
  property("Safe Float * Safe Float") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Float(1.0f), 2.0f)
  property("Safe Float * Unsafe Float") = verifyTFFloat(TwoFace.Float(2.0f) * TwoFace.Float(us(1.0f)), us(2.0f))
  property("Unsafe Float * Safe Float") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Float(1.0f), us(2.0f))
  property("Unsafe Float * Unsafe Float") = verifyTFFloat(TwoFace.Float(us(2.0f)) * TwoFace.Float(us(1.0f)), us(2.0f))
  property("Safe Float * Safe Double") = verifyTFDouble(TwoFace.Float(2.0f) * TwoFace.Double(1.0), 2.0)
  property("Safe Float * Unsafe Double") = verifyTFDouble(TwoFace.Float(2.0f) * TwoFace.Double(us(1.0)), us(2.0))
  property("Unsafe Float * Safe Double") = verifyTFDouble(TwoFace.Float(us(2.0f)) * TwoFace.Double(1.0), us(2.0))
  property("Unsafe Float * Unsafe Double") = verifyTFDouble(TwoFace.Float(us(2.0f)) * TwoFace.Double(us(1.0)), us(2.0))

  property("Safe Float / Safe Char") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Char('\u0002'), 3.0f)
  property("Safe Float / Unsafe Char") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Char(us('\u0002')), us(3.0f))
  property("Unsafe Float / Safe Char") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Char('\u0002'), us(3.0f))
  property("Unsafe Float / Unsafe Char") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Char(us('\u0002')), us(3.0f))
  property("Safe Float / Safe Int") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Int(2), 3.0f)
  property("Safe Float / Unsafe Int") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Int(us(2)), us(3.0f))
  property("Unsafe Float / Safe Int") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Int(2), us(3.0f))
  property("Unsafe Float / Unsafe Int") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Int(us(2)), us(3.0f))
  property("Safe Float / Safe Long") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Long(2L), 3.0f)
  property("Safe Float / Unsafe Long") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Long(us(2L)), us(3.0f))
  property("Unsafe Float / Safe Long") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Long(2L), us(3.0f))
  property("Unsafe Float / Unsafe Long") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Long(us(2L)), us(3.0f))
  property("Safe Float / Safe Float") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Float(2.0f), 3.0f)
  property("Safe Float / Unsafe Float") = verifyTFFloat(TwoFace.Float(6.0f) / TwoFace.Float(us(2.0f)), us(3.0f))
  property("Unsafe Float / Safe Float") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Float(2.0f), us(3.0f))
  property("Unsafe Float / Unsafe Float") = verifyTFFloat(TwoFace.Float(us(6.0f)) / TwoFace.Float(us(2.0f)), us(3.0f))
  property("Safe Float / Safe Double") = verifyTFDouble(TwoFace.Float(6.0f) / TwoFace.Double(2.0), 3.0)
  property("Safe Float / Unsafe Double") = verifyTFDouble(TwoFace.Float(6.0f) / TwoFace.Double(us(2.0)), us(3.0))
  property("Unsafe Float / Safe Double") = verifyTFDouble(TwoFace.Float(us(6.0f)) / TwoFace.Double(2.0), us(3.0))
  property("Unsafe Float / Unsafe Double") = verifyTFDouble(TwoFace.Float(us(6.0f)) / TwoFace.Double(us(2.0)), us(3.0))

  property("Safe Float % Safe Char") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Char('\u0004'), 3.0f)
  property("Safe Float % Unsafe Char") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Char(us('\u0004')), us(3.0f))
  property("Unsafe Float % Safe Char") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Char('\u0004'), us(3.0f))
  property("Unsafe Float % Unsafe Char") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Char(us('\u0004')), us(3.0f))
  property("Safe Float % Safe Int") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Int(4), 3.0f)
  property("Safe Float % Unsafe Int") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Int(us(4)), us(3.0f))
  property("Unsafe Float % Safe Int") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Int(4), us(3.0f))
  property("Unsafe Float % Unsafe Int") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Int(us(4)), us(3.0f))
  property("Safe Float % Safe Long") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Long(4L), 3.0f)
  property("Safe Float % Unsafe Long") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Long(us(4L)), us(3.0f))
  property("Unsafe Float % Safe Long") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Long(4L), us(3.0f))
  property("Unsafe Float % Unsafe Long") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Long(us(4L)), us(3.0f))
  property("Safe Float % Safe Float") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Float(4.0f), 3.0f)
  property("Safe Float % Unsafe Float") = verifyTFFloat(TwoFace.Float(7.0f) % TwoFace.Float(us(4.0f)), us(3.0f))
  property("Unsafe Float % Safe Float") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Float(4.0f), us(3.0f))
  property("Unsafe Float % Unsafe Float") = verifyTFFloat(TwoFace.Float(us(7.0f)) % TwoFace.Float(us(4.0f)), us(3.0f))
  property("Safe Float % Safe Double") = verifyTFDouble(TwoFace.Float(7.0f) % TwoFace.Double(4.0), 3.0)
  property("Safe Float % Unsafe Double") = verifyTFDouble(TwoFace.Float(7.0f) % TwoFace.Double(us(4.0)), us(3.0))
  property("Unsafe Float % Safe Double") = verifyTFDouble(TwoFace.Float(us(7.0f)) % TwoFace.Double(4.0), us(3.0))
  property("Unsafe Float % Unsafe Double") = verifyTFDouble(TwoFace.Float(us(7.0f)) % TwoFace.Double(us(4.0)), us(3.0))

  property("Safe Float < Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Char('\u0004'), false)
  property("Safe Float < Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Float < Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Char('\u0004'), us(false))
  property("Unsafe Float < Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Char(us('\u0004')), us(false))
  property("Safe Float < Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Int(4), false)
  property("Safe Float < Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Int(us(4)), us(false))
  property("Unsafe Float < Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Int(4), us(false))
  property("Unsafe Float < Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Int(us(4)), us(false))
  property("Safe Float < Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Long(4L), false)
  property("Safe Float < Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Long(us(4L)), us(false))
  property("Unsafe Float < Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Long(4L), us(false))
  property("Unsafe Float < Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Long(us(4L)), us(false))
  property("Safe Float < Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Float(4.0f), false)
  property("Safe Float < Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Float < Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Float(4.0f), us(false))
  property("Unsafe Float < Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Float(us(4.0f)), us(false))
  property("Safe Float < Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Double(4.0), false)
  property("Safe Float < Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) < TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Float < Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Double(4.0), us(false))
  property("Unsafe Float < Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) < TwoFace.Double(us(4.0)), us(false))

  property("Safe Float > Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Char('\u0004'), true)
  property("Safe Float > Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Float > Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Char('\u0004'), us(true))
  property("Unsafe Float > Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Char(us('\u0004')), us(true))
  property("Safe Float > Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Int(4), true)
  property("Safe Float > Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Int(us(4)), us(true))
  property("Unsafe Float > Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Int(4), us(true))
  property("Unsafe Float > Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Int(us(4)), us(true))
  property("Safe Float > Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Long(4L), true)
  property("Safe Float > Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Long(us(4L)), us(true))
  property("Unsafe Float > Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Long(4L), us(true))
  property("Unsafe Float > Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Long(us(4L)), us(true))
  property("Safe Float > Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Float(4.0f), true)
  property("Safe Float > Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Float > Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Float(4.0f), us(true))
  property("Unsafe Float > Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Float(us(4.0f)), us(true))
  property("Safe Float > Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Double(4.0), true)
  property("Safe Float > Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) > TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Float > Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Double(4.0), us(true))
  property("Unsafe Float > Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) > TwoFace.Double(us(4.0)), us(true))

  property("Safe Float <= Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Char('\u0004'), false)
  property("Safe Float <= Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Float <= Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Char('\u0004'), us(false))
  property("Unsafe Float <= Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Char(us('\u0004')), us(false))
  property("Safe Float <= Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Int(4), false)
  property("Safe Float <= Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Int(us(4)), us(false))
  property("Unsafe Float <= Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Int(4), us(false))
  property("Unsafe Float <= Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Int(us(4)), us(false))
  property("Safe Float <= Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Long(4L), false)
  property("Safe Float <= Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Long(us(4L)), us(false))
  property("Unsafe Float <= Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Long(4L), us(false))
  property("Unsafe Float <= Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Long(us(4L)), us(false))
  property("Safe Float <= Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Float(4.0f), false)
  property("Safe Float <= Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Float <= Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Float(4.0f), us(false))
  property("Unsafe Float <= Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Float(us(4.0f)), us(false))
  property("Safe Float <= Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Double(4.0), false)
  property("Safe Float <= Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) <= TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Float <= Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Double(4.0), us(false))
  property("Unsafe Float <= Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) <= TwoFace.Double(us(4.0)), us(false))

  property("Safe Float >= Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Char('\u0004'), true)
  property("Safe Float >= Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Float >= Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Char('\u0004'), us(true))
  property("Unsafe Float >= Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Char(us('\u0004')), us(true))
  property("Safe Float >= Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Int(4), true)
  property("Safe Float >= Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Int(us(4)), us(true))
  property("Unsafe Float >= Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Int(4), us(true))
  property("Unsafe Float >= Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Int(us(4)), us(true))
  property("Safe Float >= Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Long(4L), true)
  property("Safe Float >= Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Long(us(4L)), us(true))
  property("Unsafe Float >= Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Long(4L), us(true))
  property("Unsafe Float >= Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Long(us(4L)), us(true))
  property("Safe Float >= Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Float(4.0f), true)
  property("Safe Float >= Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Float >= Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Float(4.0f), us(true))
  property("Unsafe Float >= Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Float(us(4.0f)), us(true))
  property("Safe Float >= Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Double(4.0), true)
  property("Safe Float >= Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) >= TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Float >= Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Double(4.0), us(true))
  property("Unsafe Float >= Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) >= TwoFace.Double(us(4.0)), us(true))

  property("Safe Float == Regular Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) == ('\u0007'), true)
  property("Safe Float == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) == (us('\u0007')), us(true))
  property("Unsafe Float == Regular Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == ('\u0007'), us(true))
  property("Unsafe Float == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (us('\u0007')), us(true))
  property("Safe Float == Regular Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) == (7), true)
  property("Safe Float == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) == (us(7)), us(true))
  property("Unsafe Float == Regular Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (7), us(true))
  property("Unsafe Float == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (us(7)), us(true))
  property("Safe Float == Regular Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) == (7L), true)
  property("Safe Float == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) == (us(7L)), us(true))
  property("Unsafe Float == Regular Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (7L), us(true))
  property("Unsafe Float == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (us(7L)), us(true))
  property("Safe Float == Regular Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) == (7.0f), true)
  property("Safe Float == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) == (us(7.0f)), us(true))
  property("Unsafe Float == Regular Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (7.0f), us(true))
  property("Unsafe Float == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (us(7.0f)), us(true))
  property("Safe Float == Regular Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) == (7.0), true)
  property("Safe Float == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) == (us(7.0)), us(true))
  property("Unsafe Float == Regular Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (7.0), us(true))
  property("Unsafe Float == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == (us(7.0)), us(true))

  property("Safe Float != Regular Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) != ('\u0007'), false)
  property("Safe Float != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) != (us('\u0007')), us(false))
  property("Unsafe Float != Regular Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != ('\u0007'), us(false))
  property("Unsafe Float != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (us('\u0007')), us(false))
  property("Safe Float != Regular Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) != (7), false)
  property("Safe Float != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) != (us(7)), us(false))
  property("Unsafe Float != Regular Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (7), us(false))
  property("Unsafe Float != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (us(7)), us(false))
  property("Safe Float != Regular Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) != (7L), false)
  property("Safe Float != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) != (us(7L)), us(false))
  property("Unsafe Float != Regular Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (7L), us(false))
  property("Unsafe Float != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (us(7L)), us(false))
  property("Safe Float != Regular Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) != (7.0f), false)
  property("Safe Float != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) != (us(7.0f)), us(false))
  property("Unsafe Float != Regular Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (7.0f), us(false))
  property("Unsafe Float != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (us(7.0f)), us(false))
  property("Safe Float != Regular Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) != (7.0), false)
  property("Safe Float != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) != (us(7.0)), us(false))
  property("Unsafe Float != Regular Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (7.0), us(false))
  property("Unsafe Float != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != (us(7.0)), us(false))

  property("Safe Float == Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Char('\u0007'), true)
  property("Safe Float == Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Char(us('\u0007')), us(true))
  property("Unsafe Float == Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Char('\u0007'), us(true))
  property("Unsafe Float == Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Char(us('\u0007')), us(true))
  property("Safe Float == Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Int(7), true)
  property("Safe Float == Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Int(us(7)), us(true))
  property("Unsafe Float == Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Int(7), us(true))
  property("Unsafe Float == Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Int(us(7)), us(true))
  property("Safe Float == Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Long(7L), true)
  property("Safe Float == Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Long(us(7L)), us(true))
  property("Unsafe Float == Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Long(7L), us(true))
  property("Unsafe Float == Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Long(us(7L)), us(true))
  property("Safe Float == Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Float(7.0f), true)
  property("Safe Float == Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Float(us(7.0f)), us(true))
  property("Unsafe Float == Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Float(7.0f), us(true))
  property("Unsafe Float == Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Float(us(7.0f)), us(true))
  property("Safe Float == Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Double(7.0), true)
  property("Safe Float == Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) == TwoFace.Double(us(7.0)), us(true))
  property("Unsafe Float == Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Double(7.0), us(true))
  property("Unsafe Float == Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) == TwoFace.Double(us(7.0)), us(true))

  property("Safe Float != Safe Char") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Char('\u0007'), false)
  property("Safe Float != Unsafe Char") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Char(us('\u0007')), us(false))
  property("Unsafe Float != Safe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Char('\u0007'), us(false))
  property("Unsafe Float != Unsafe Char") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Char(us('\u0007')), us(false))
  property("Safe Float != Safe Int") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Int(7), false)
  property("Safe Float != Unsafe Int") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Int(us(7)), us(false))
  property("Unsafe Float != Safe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Int(7), us(false))
  property("Unsafe Float != Unsafe Int") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Int(us(7)), us(false))
  property("Safe Float != Safe Long") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Long(7L), false)
  property("Safe Float != Unsafe Long") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Long(us(7L)), us(false))
  property("Unsafe Float != Safe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Long(7L), us(false))
  property("Unsafe Float != Unsafe Long") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Long(us(7L)), us(false))
  property("Safe Float != Safe Float") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Float(7.0f), false)
  property("Safe Float != Unsafe Float") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Float(us(7.0f)), us(false))
  property("Unsafe Float != Safe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Float(7.0f), us(false))
  property("Unsafe Float != Unsafe Float") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Float(us(7.0f)), us(false))
  property("Safe Float != Safe Double") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Double(7.0), false)
  property("Safe Float != Unsafe Double") = verifyTFBoolean(TwoFace.Float(7.0f) != TwoFace.Double(us(7.0)), us(false))
  property("Unsafe Float != Safe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Double(7.0), us(false))
  property("Unsafe Float != Unsafe Double") = verifyTFBoolean(TwoFace.Float(us(7.0f)) != TwoFace.Double(us(7.0)), us(false))

  property("Safe Float, Safe Float") = verifyTFFloat(min(TwoFace.Float(2.0f), TwoFace.Float(1.0f)), 1.0f)
  property("Safe Float, Unsafe Float") = verifyTFFloat(min(TwoFace.Float(2.0f), TwoFace.Float(us(1.0f))), us(1.0f))
  property("Unsafe Float, Safe Float") = verifyTFFloat(min(TwoFace.Float(us(2.0f)), TwoFace.Float(1.0f)), us(1.0f))
  property("Unsafe Float, Unsafe Float") = verifyTFFloat(min(TwoFace.Float(us(2.0f)), TwoFace.Float(us(1.0f))), us(1.0f))

  property("Safe Float, Safe Float") = verifyTFFloat(max(TwoFace.Float(2.0f), TwoFace.Float(1.0f)), 2.0f)
  property("Safe Float, Unsafe Float") = verifyTFFloat(max(TwoFace.Float(2.0f), TwoFace.Float(us(1.0f))), us(2.0f))
  property("Unsafe Float, Safe Float") = verifyTFFloat(max(TwoFace.Float(us(2.0f)), TwoFace.Float(1.0f)), us(2.0f))
  property("Unsafe Float, Unsafe Float") = verifyTFFloat(max(TwoFace.Float(us(2.0f)), TwoFace.Float(us(1.0f))), us(2.0f))

  property("Safe Negate") = verifyTFFloat(-TwoFace.Float(-1.0f), 1.0f)
  property("Unsafe Negate") = verifyTFFloat(-TwoFace.Float(us(1.0f)), us(-1.0f))

  property("Safe toNat") = wellTyped {
    val nat = TwoFace.Float(2.0f).toNat
    verifyOp[nat.N, shapeless.Nat._2]
  }
  property("Safe toChar") = verifyTFChar(TwoFace.Float(1.0f).toChar, '\u0001')
  property("Unsafe toChar") = verifyTFChar(TwoFace.Float(us(1.0f)).toChar, us('\u0001'))
  property("Safe toInt") = verifyTFInt(TwoFace.Float(1.0f).toInt, 1)
  property("Unsafe toInt") = verifyTFInt(TwoFace.Float(us(1.0f)).toInt, us(1))
  property("Safe toLong") = verifyTFLong(TwoFace.Float(1.0f).toLong, 1L)
  property("Unsafe toLong") = verifyTFLong(TwoFace.Float(us(1.0f)).toLong, us(1L))
  property("Safe toDouble") = verifyTFDouble(TwoFace.Float(1.0f).toDouble, 1.0)
  property("Unsafe toDouble") = verifyTFDouble(TwoFace.Float(us(1.0f)).toDouble, us(1.0))
  property("Safe toStringTF") = verifyTFString(TwoFace.Float(1.0f).toStringTF, "1.0")
  property("Unsafe toStringTF") = verifyTFString(TwoFace.Float(us(1.5f)).toStringTF, us("1.5"))
  property("Safe toSymbol") = {
    val sym = TwoFace.Float(2.0f).toSymbol
    sym == scala.Symbol("2.0")
  }

  property("Safe abs") = verifyTFFloat(abs(TwoFace.Float(-1.0f)), 1.0f)
  property("Unsafe abs") = verifyTFFloat(abs(TwoFace.Float(us(-1.0f))), us(1.0f))

  property("Safe sin") = verifyTFDouble(sin(TwoFace.Float(1.0f)), 0.8414709848078965)
  property("Unsafe sin") = verifyTFDouble(sin(TwoFace.Float(us(1.0f))), us(0.8414709848078965))
  property("Safe cos") = verifyTFDouble(cos(TwoFace.Float(1.0f)), 0.5403023058681398)
  property("Unsafe cos") = verifyTFDouble(cos(TwoFace.Float(us(1.0f))), us(0.5403023058681398))
  property("Safe tan") = verifyTFDouble(tan(TwoFace.Float(1.0f)), 1.5574077246549023)
  property("Unsafe tan") = verifyTFDouble(tan(TwoFace.Float(us(1.0f))), us(1.5574077246549023))
  property("Safe ceil") = verifyTFDouble(ceil(TwoFace.Float(1.5f)), 2.0)
  property("Unsafe ceil") = verifyTFDouble(ceil(TwoFace.Float(us(1.5f))), us(2.0))
  property("Safe floor") = verifyTFDouble(floor(TwoFace.Float(1.5f)), 1.0)
  property("Unsafe floor") = verifyTFDouble(floor(TwoFace.Float(us(1.5f))), us(1.0))
  property("Safe round") = verifyTFInt(round(TwoFace.Float(1.5f)), 2)
  property("Unsafe round") = verifyTFInt(round(TwoFace.Float(us(1.5f))), us(2))
  property("Safe sqrt") = verifyTFDouble(sqrt(TwoFace.Float(9.0f)), 3.0)
  property("Unsafe sqrt") = verifyTFDouble(sqrt(TwoFace.Float(us(9.0f))), us(3.0))
  property("Safe log") = verifyTFDouble(log(TwoFace.Float(9.0f)), 2.1972245773362196)
  property("Unsafe log") = verifyTFDouble(log(TwoFace.Float(us(9.0f))), us(2.1972245773362196))
  property("Safe log10") = verifyTFDouble(log10(TwoFace.Float(9.0f)), 0.9542425094393249)
  property("Unsafe log10") = verifyTFDouble(log10(TwoFace.Float(us(9.0f))), us(0.9542425094393249))

  property("Safe Float pow Safe Float") = verifyTFDouble(pow(TwoFace.Float(2.0f), TwoFace.Float(3.0f)), 8.0)
  property("Safe Float pow Unsafe Float") = verifyTFDouble(pow(TwoFace.Float(2.0f), TwoFace.Float(us(3.0f))), us(8.0))
  property("Unsafe Float pow Safe Float") = verifyTFDouble(pow(TwoFace.Float(us(2.0f)), TwoFace.Float(3.0f)), us(8.0))
  property("Unsafe Float pow Unsafe Float") = verifyTFDouble(pow(TwoFace.Float(us(2.0f)), TwoFace.Float(us(3.0f))), us(8.0))
  property("Safe Float pow Safe Double") = verifyTFDouble(pow(TwoFace.Float(2.0f), TwoFace.Double(3.0)), 8.0)
  property("Safe Float pow Unsafe Double") = verifyTFDouble(pow(TwoFace.Float(2.0f), TwoFace.Double(us(3.0))), us(8.0))
  property("Unsafe Float pow Safe Double") = verifyTFDouble(pow(TwoFace.Float(us(2.0f)), TwoFace.Double(3.0)), us(8.0))
  property("Unsafe Float pow Unsafe Double") = verifyTFDouble(pow(TwoFace.Float(us(2.0f)), TwoFace.Double(us(3.0))), us(8.0))

  property("Implicit Conversions") = wellTyped {
    val a : TwoFace.Float[W.`3.0f`.T] = implicitly[TwoFace.Float[W.`2.0f`.T + W.`1.0f`.T]]
    val b : TwoFace.Float[W.`3.0f`.T + W.`0.0f`.T] = implicitly[TwoFace.Float[W.`2.0f`.T + W.`1.0f`.T]]
    val c : TwoFace.Float[W.`3.0f`.T + W.`0.0f`.T] = implicitly[TwoFace.Float[W.`3.0f`.T]]
    val d : W.`3.0f`.T = TwoFace.Float(3.0f)
    val e : Float = TwoFace.Float(us(3.0f))
    val f : TwoFace.Float[Float] = 3.0f
  }

  property("Wrong Implicit Conversions") = wellTyped {
    illTyped("""val impl = implicitly[TwoFace.Float[W.`2.0f`.T + W.`2.0f`.T]]; val a : TwoFace.Float[W.`3.0f`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.Float[W.`2.0f`.T + W.`2.0f`.T]]; val b : TwoFace.Float[W.`3.0f`.T + W.`0.0f`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.Float[W.`4.0f`.T]]; val c : TwoFace.Float[W.`3.0f`.T + W.`0.0f`.T] = impl""")
  }

  property("ToString") = {
    TwoFace.Float[W.`1.5f`.T].toString() == "1.5"
  }

  type Fin = W.`3.0f`.T
  final val fin = 3.0f

  property("Extracting from an Upper Bounded Numeric") = wellTyped {
    def foo[W](width: TwoFace.Float[W]) = width
    def foo2[R <: Float](r: R) = foo(r)
    val a = foo2(W(fin).value)
    implicitly[a.Out =:= Fin]
    val b = foo2(us(fin))
    implicitly[b.Out =:= Float]
  }

  property("Extracting from Safe TwoFace") = {
    val a = me(TwoFace.Float(fin))
    val ret = shapeless.the[Id[a.T]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  def noImplFoo[W](w : TwoFace.Float[W]) = -w //Missing twoface shell implicit
  property("Unavailable Implicit Safe TwoFace Shell") = {
    val ret = noImplFoo(2.0f)
    implicitly[ret.Out <:< Negate[W.`2.0f`.T]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< W.`-2.0f`.T]
    retSimple.getValue == -2.0f
  }
  property("Unavailable Implicit Unsafe TwoFace Shell") = {
    val ret = noImplFoo(us(2.0f))
    implicitly[ret.Out <:< Negate[Float]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< Float]
    retSimple.getValue == -2.0f
  }
}
