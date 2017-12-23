package singleton.twoface

import singleton.twoface.math._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class TwoFaceDoubleSpec extends Properties("TwoFace.Double") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.Double[W.`2.0`.T]]
    a.getValue == 2.0 && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.Double[W.`2.0`.T]
    a.getValue == 2.0 && a.isLiteral
  }
  property("Safe Creation()") = {
    val a = TwoFace.Double(2.0)
    a.getValue == 2.0 && a.isLiteral
  }
  property("Unsafe Creation()") = {
    val a = TwoFace.Double(us(2.0))
    a.getValue == 2.0 && !a.isLiteral
  }

  property("Safe ifThenElse") = verifyTFDouble(ifThenElse(true, 1.0, 2.0), 1.0)
  property("Unsafe ifThenElse") = verifyTFDouble(ifThenElse(us(false), 1.0, 2.0), us(2.0))

  property("Pi") = verifyTFDouble(Pi, 3.141592653589793)
  property("E") = verifyTFDouble(E, 2.718281828459045)

  property("Safe Double + Safe Char") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Char('\u0001'), 3.0)
  property("Safe Double + Unsafe Char") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Char(us('\u0001')), us(3.0))
  property("Unsafe Double + Safe Char") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Char('\u0001'), us(3.0))
  property("Unsafe Double + Unsafe Char") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Char(us('\u0001')), us(3.0))
  property("Safe Double + Safe Int") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Int(1), 3.0)
  property("Safe Double + Unsafe Int") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Int(us(1)), us(3.0))
  property("Unsafe Double + Safe Int") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Int(1), us(3.0))
  property("Unsafe Double + Unsafe Int") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Int(us(1)), us(3.0))
  property("Safe Double + Safe Long") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Long(1L), 3.0)
  property("Safe Double + Unsafe Long") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Long(us(1L)), us(3.0))
  property("Unsafe Double + Safe Long") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Long(1L), us(3.0))
  property("Unsafe Double + Unsafe Long") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Long(us(1L)), us(3.0))
  property("Safe Double + Safe Float") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Float(1.0f), 3.0)
  property("Safe Double + Unsafe Float") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Float(us(1.0f)), us(3.0))
  property("Unsafe Double + Safe Float") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Float(1.0f), us(3.0))
  property("Unsafe Double + Unsafe Float") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Float(us(1.0f)), us(3.0))
  property("Safe Double + Safe Double") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Double(1.0), 3.0)
  property("Safe Double + Unsafe Double") = verifyTFDouble(TwoFace.Double(2.0) + TwoFace.Double(us(1.0)), us(3.0))
  property("Unsafe Double + Safe Double") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Double(1.0), us(3.0))
  property("Unsafe Double + Unsafe Double") = verifyTFDouble(TwoFace.Double(us(2.0)) + TwoFace.Double(us(1.0)), us(3.0))

  property("Safe Double - Safe Char") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Char('\u0001'), 1.0)
  property("Safe Double - Unsafe Char") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Char(us('\u0001')), us(1.0))
  property("Unsafe Double - Safe Char") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Char('\u0001'), us(1.0))
  property("Unsafe Double - Unsafe Char") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Char(us('\u0001')), us(1.0))
  property("Safe Double - Safe Int") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Int(1), 1.0)
  property("Safe Double - Unsafe Int") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Int(us(1)), us(1.0))
  property("Unsafe Double - Safe Int") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Int(1), us(1.0))
  property("Unsafe Double - Unsafe Int") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Int(us(1)), us(1.0))
  property("Safe Double - Safe Long") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Long(1L), 1.0)
  property("Safe Double - Unsafe Long") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Long(us(1L)), us(1.0))
  property("Unsafe Double - Safe Long") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Long(1L), us(1.0))
  property("Unsafe Double - Unsafe Long") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Long(us(1L)), us(1.0))
  property("Safe Double - Safe Float") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Float(1.0f), 1.0)
  property("Safe Double - Unsafe Float") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Float(us(1.0f)), us(1.0))
  property("Unsafe Double - Safe Float") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Float(1.0f), us(1.0))
  property("Unsafe Double - Unsafe Float") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Float(us(1.0f)), us(1.0))
  property("Safe Double - Safe Double") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Double(1.0), 1.0)
  property("Safe Double - Unsafe Double") = verifyTFDouble(TwoFace.Double(2.0) - TwoFace.Double(us(1.0)), us(1.0))
  property("Unsafe Double - Safe Double") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Double(1.0), us(1.0))
  property("Unsafe Double - Unsafe Double") = verifyTFDouble(TwoFace.Double(us(2.0)) - TwoFace.Double(us(1.0)), us(1.0))

  property("Safe Double * Safe Char") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Char('\u0001'), 2.0)
  property("Safe Double * Unsafe Char") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Char(us('\u0001')), us(2.0))
  property("Unsafe Double * Safe Char") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Char('\u0001'), us(2.0))
  property("Unsafe Double * Unsafe Char") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Char(us('\u0001')), us(2.0))
  property("Safe Double * Safe Int") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Int(1), 2.0)
  property("Safe Double * Unsafe Int") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Int(us(1)), us(2.0))
  property("Unsafe Double * Safe Int") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Int(1), us(2.0))
  property("Unsafe Double * Unsafe Int") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Int(us(1)), us(2.0))
  property("Safe Double * Safe Long") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Long(1L), 2.0)
  property("Safe Double * Unsafe Long") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Long(us(1L)), us(2.0))
  property("Unsafe Double * Safe Long") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Long(1L), us(2.0))
  property("Unsafe Double * Unsafe Long") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Long(us(1L)), us(2.0))
  property("Safe Double * Safe Float") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Float(1.0f), 2.0)
  property("Safe Double * Unsafe Float") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Float(us(1.0f)), us(2.0))
  property("Unsafe Double * Safe Float") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Float(1.0f), us(2.0))
  property("Unsafe Double * Unsafe Float") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Float(us(1.0f)), us(2.0))
  property("Safe Double * Safe Double") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Double(1.0), 2.0)
  property("Safe Double * Unsafe Double") = verifyTFDouble(TwoFace.Double(2.0) * TwoFace.Double(us(1.0)), us(2.0))
  property("Unsafe Double * Safe Double") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Double(1.0), us(2.0))
  property("Unsafe Double * Unsafe Double") = verifyTFDouble(TwoFace.Double(us(2.0)) * TwoFace.Double(us(1.0)), us(2.0))

  property("Safe Double / Safe Char") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Char('\u0002'), 3.0)
  property("Safe Double / Unsafe Char") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Char(us('\u0002')), us(3.0))
  property("Unsafe Double / Safe Char") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Char('\u0002'), us(3.0))
  property("Unsafe Double / Unsafe Char") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Char(us('\u0002')), us(3.0))
  property("Safe Double / Safe Int") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Int(2), 3.0)
  property("Safe Double / Unsafe Int") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Int(us(2)), us(3.0))
  property("Unsafe Double / Safe Int") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Int(2), us(3.0))
  property("Unsafe Double / Unsafe Int") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Int(us(2)), us(3.0))
  property("Safe Double / Safe Long") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Long(2L), 3.0)
  property("Safe Double / Unsafe Long") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Long(us(2L)), us(3.0))
  property("Unsafe Double / Safe Long") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Long(2L), us(3.0))
  property("Unsafe Double / Unsafe Long") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Long(us(2L)), us(3.0))
  property("Safe Double / Safe Float") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Float(2.0f), 3.0)
  property("Safe Double / Unsafe Float") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Float(us(2.0f)), us(3.0))
  property("Unsafe Double / Safe Float") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Float(2.0f), us(3.0))
  property("Unsafe Double / Unsafe Float") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Float(us(2.0f)), us(3.0))
  property("Safe Double / Safe Double") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Double(2.0), 3.0)
  property("Safe Double / Unsafe Double") = verifyTFDouble(TwoFace.Double(6.0) / TwoFace.Double(us(2.0)), us(3.0))
  property("Unsafe Double / Safe Double") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Double(2.0), us(3.0))
  property("Unsafe Double / Unsafe Double") = verifyTFDouble(TwoFace.Double(us(6.0)) / TwoFace.Double(us(2.0)), us(3.0))

  property("Safe Double % Safe Char") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Char('\u0004'), 3.0)
  property("Safe Double % Unsafe Char") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Char(us('\u0004')), us(3.0))
  property("Unsafe Double % Safe Char") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Char('\u0004'), us(3.0))
  property("Unsafe Double % Unsafe Char") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Char(us('\u0004')), us(3.0))
  property("Safe Double % Safe Int") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Int(4), 3.0)
  property("Safe Double % Unsafe Int") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Int(us(4)), us(3.0))
  property("Unsafe Double % Safe Int") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Int(4), us(3.0))
  property("Unsafe Double % Unsafe Int") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Int(us(4)), us(3.0))
  property("Safe Double % Safe Long") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Long(4L), 3.0)
  property("Safe Double % Unsafe Long") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Long(us(4L)), us(3.0))
  property("Unsafe Double % Safe Long") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Long(4L), us(3.0))
  property("Unsafe Double % Unsafe Long") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Long(us(4L)), us(3.0))
  property("Safe Double % Safe Float") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Float(4.0f), 3.0)
  property("Safe Double % Unsafe Float") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Float(us(4.0f)), us(3.0))
  property("Unsafe Double % Safe Float") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Float(4.0f), us(3.0))
  property("Unsafe Double % Unsafe Float") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Float(us(4.0f)), us(3.0))
  property("Safe Double % Safe Double") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Double(4.0), 3.0)
  property("Safe Double % Unsafe Double") = verifyTFDouble(TwoFace.Double(7.0) % TwoFace.Double(us(4.0)), us(3.0))
  property("Unsafe Double % Safe Double") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Double(4.0), us(3.0))
  property("Unsafe Double % Unsafe Double") = verifyTFDouble(TwoFace.Double(us(7.0)) % TwoFace.Double(us(4.0)), us(3.0))

  property("Safe Double < Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Char('\u0004'), false)
  property("Safe Double < Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Double < Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Char('\u0004'), us(false))
  property("Unsafe Double < Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Char(us('\u0004')), us(false))
  property("Safe Double < Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Int(4), false)
  property("Safe Double < Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Int(us(4)), us(false))
  property("Unsafe Double < Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Int(4), us(false))
  property("Unsafe Double < Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Int(us(4)), us(false))
  property("Safe Double < Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Long(4L), false)
  property("Safe Double < Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Long(us(4L)), us(false))
  property("Unsafe Double < Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Long(4L), us(false))
  property("Unsafe Double < Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Long(us(4L)), us(false))
  property("Safe Double < Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Float(4.0f), false)
  property("Safe Double < Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Double < Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Float(4.0f), us(false))
  property("Unsafe Double < Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Float(us(4.0f)), us(false))
  property("Safe Double < Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Double(4.0), false)
  property("Safe Double < Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) < TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Double < Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Double(4.0), us(false))
  property("Unsafe Double < Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) < TwoFace.Double(us(4.0)), us(false))

  property("Safe Double > Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Char('\u0004'), true)
  property("Safe Double > Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Double > Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Char('\u0004'), us(true))
  property("Unsafe Double > Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Char(us('\u0004')), us(true))
  property("Safe Double > Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Int(4), true)
  property("Safe Double > Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Int(us(4)), us(true))
  property("Unsafe Double > Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Int(4), us(true))
  property("Unsafe Double > Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Int(us(4)), us(true))
  property("Safe Double > Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Long(4L), true)
  property("Safe Double > Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Long(us(4L)), us(true))
  property("Unsafe Double > Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Long(4L), us(true))
  property("Unsafe Double > Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Long(us(4L)), us(true))
  property("Safe Double > Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Float(4.0f), true)
  property("Safe Double > Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Double > Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Float(4.0f), us(true))
  property("Unsafe Double > Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Float(us(4.0f)), us(true))
  property("Safe Double > Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Double(4.0), true)
  property("Safe Double > Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) > TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Double > Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Double(4.0), us(true))
  property("Unsafe Double > Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) > TwoFace.Double(us(4.0)), us(true))

  property("Safe Double <= Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Char('\u0004'), false)
  property("Safe Double <= Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Double <= Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Char('\u0004'), us(false))
  property("Unsafe Double <= Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Char(us('\u0004')), us(false))
  property("Safe Double <= Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Int(4), false)
  property("Safe Double <= Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Int(us(4)), us(false))
  property("Unsafe Double <= Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Int(4), us(false))
  property("Unsafe Double <= Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Int(us(4)), us(false))
  property("Safe Double <= Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Long(4L), false)
  property("Safe Double <= Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Long(us(4L)), us(false))
  property("Unsafe Double <= Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Long(4L), us(false))
  property("Unsafe Double <= Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Long(us(4L)), us(false))
  property("Safe Double <= Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Float(4.0f), false)
  property("Safe Double <= Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Double <= Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Float(4.0f), us(false))
  property("Unsafe Double <= Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Float(us(4.0f)), us(false))
  property("Safe Double <= Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Double(4.0), false)
  property("Safe Double <= Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) <= TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Double <= Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Double(4.0), us(false))
  property("Unsafe Double <= Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) <= TwoFace.Double(us(4.0)), us(false))

  property("Safe Double >= Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Char('\u0004'), true)
  property("Safe Double >= Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Double >= Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Char('\u0004'), us(true))
  property("Unsafe Double >= Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Char(us('\u0004')), us(true))
  property("Safe Double >= Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Int(4), true)
  property("Safe Double >= Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Int(us(4)), us(true))
  property("Unsafe Double >= Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Int(4), us(true))
  property("Unsafe Double >= Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Int(us(4)), us(true))
  property("Safe Double >= Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Long(4L), true)
  property("Safe Double >= Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Long(us(4L)), us(true))
  property("Unsafe Double >= Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Long(4L), us(true))
  property("Unsafe Double >= Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Long(us(4L)), us(true))
  property("Safe Double >= Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Float(4.0f), true)
  property("Safe Double >= Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Double >= Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Float(4.0f), us(true))
  property("Unsafe Double >= Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Float(us(4.0f)), us(true))
  property("Safe Double >= Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Double(4.0), true)
  property("Safe Double >= Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) >= TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Double >= Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Double(4.0), us(true))
  property("Unsafe Double >= Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) >= TwoFace.Double(us(4.0)), us(true))

  property("Safe Double == Regular Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) == ('\u0007'), true)
  property("Safe Double == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) == (us('\u0007')), us(true))
  property("Unsafe Double == Regular Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) == ('\u0007'), us(true))
  property("Unsafe Double == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (us('\u0007')), us(true))
  property("Safe Double == Regular Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) == (7), true)
  property("Safe Double == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) == (us(7)), us(true))
  property("Unsafe Double == Regular Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (7), us(true))
  property("Unsafe Double == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (us(7)), us(true))
  property("Safe Double == Regular Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) == (7L), true)
  property("Safe Double == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) == (us(7L)), us(true))
  property("Unsafe Double == Regular Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (7L), us(true))
  property("Unsafe Double == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (us(7L)), us(true))
  property("Safe Double == Regular Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) == (7.0f), true)
  property("Safe Double == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) == (us(7.0f)), us(true))
  property("Unsafe Double == Regular Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (7.0f), us(true))
  property("Unsafe Double == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (us(7.0f)), us(true))
  property("Safe Double == Regular Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) == (7.0), true)
  property("Safe Double == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) == (us(7.0)), us(true))
  property("Unsafe Double == Regular Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (7.0), us(true))
  property("Unsafe Double == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) == (us(7.0)), us(true))

  property("Safe Double != Regular Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) != ('\u0007'), false)
  property("Safe Double != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) != (us('\u0007')), us(false))
  property("Unsafe Double != Regular Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) != ('\u0007'), us(false))
  property("Unsafe Double != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (us('\u0007')), us(false))
  property("Safe Double != Regular Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) != (7), false)
  property("Safe Double != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) != (us(7)), us(false))
  property("Unsafe Double != Regular Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (7), us(false))
  property("Unsafe Double != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (us(7)), us(false))
  property("Safe Double != Regular Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) != (7L), false)
  property("Safe Double != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) != (us(7L)), us(false))
  property("Unsafe Double != Regular Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (7L), us(false))
  property("Unsafe Double != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (us(7L)), us(false))
  property("Safe Double != Regular Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) != (7.0f), false)
  property("Safe Double != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) != (us(7.0f)), us(false))
  property("Unsafe Double != Regular Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (7.0f), us(false))
  property("Unsafe Double != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (us(7.0f)), us(false))
  property("Safe Double != Regular Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) != (7.0), false)
  property("Safe Double != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) != (us(7.0)), us(false))
  property("Unsafe Double != Regular Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (7.0), us(false))
  property("Unsafe Double != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) != (us(7.0)), us(false))

  property("Safe Double == Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Char('\u0007'), true)
  property("Safe Double == Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Char(us('\u0007')), us(true))
  property("Unsafe Double == Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Char('\u0007'), us(true))
  property("Unsafe Double == Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Char(us('\u0007')), us(true))
  property("Safe Double == Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Int(7), true)
  property("Safe Double == Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Int(us(7)), us(true))
  property("Unsafe Double == Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Int(7), us(true))
  property("Unsafe Double == Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Int(us(7)), us(true))
  property("Safe Double == Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Long(7L), true)
  property("Safe Double == Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Long(us(7L)), us(true))
  property("Unsafe Double == Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Long(7L), us(true))
  property("Unsafe Double == Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Long(us(7L)), us(true))
  property("Safe Double == Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Float(7.0f), true)
  property("Safe Double == Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Float(us(7.0f)), us(true))
  property("Unsafe Double == Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Float(7.0f), us(true))
  property("Unsafe Double == Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Float(us(7.0f)), us(true))
  property("Safe Double == Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Double(7.0), true)
  property("Safe Double == Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) == TwoFace.Double(us(7.0)), us(true))
  property("Unsafe Double == Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Double(7.0), us(true))
  property("Unsafe Double == Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) == TwoFace.Double(us(7.0)), us(true))

  property("Safe Double != Safe Char") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Char('\u0007'), false)
  property("Safe Double != Unsafe Char") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Char(us('\u0007')), us(false))
  property("Unsafe Double != Safe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Char('\u0007'), us(false))
  property("Unsafe Double != Unsafe Char") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Char(us('\u0007')), us(false))
  property("Safe Double != Safe Int") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Int(7), false)
  property("Safe Double != Unsafe Int") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Int(us(7)), us(false))
  property("Unsafe Double != Safe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Int(7), us(false))
  property("Unsafe Double != Unsafe Int") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Int(us(7)), us(false))
  property("Safe Double != Safe Long") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Long(7L), false)
  property("Safe Double != Unsafe Long") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Long(us(7L)), us(false))
  property("Unsafe Double != Safe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Long(7L), us(false))
  property("Unsafe Double != Unsafe Long") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Long(us(7L)), us(false))
  property("Safe Double != Safe Float") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Float(7.0f), false)
  property("Safe Double != Unsafe Float") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Float(us(7.0f)), us(false))
  property("Unsafe Double != Safe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Float(7.0f), us(false))
  property("Unsafe Double != Unsafe Float") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Float(us(7.0f)), us(false))
  property("Safe Double != Safe Double") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Double(7.0), false)
  property("Safe Double != Unsafe Double") = verifyTFBoolean(TwoFace.Double(7.0) != TwoFace.Double(us(7.0)), us(false))
  property("Unsafe Double != Safe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Double(7.0), us(false))
  property("Unsafe Double != Unsafe Double") = verifyTFBoolean(TwoFace.Double(us(7.0)) != TwoFace.Double(us(7.0)), us(false))

  property("Safe Double, Safe Double") = verifyTFDouble(min(TwoFace.Double(2.0), TwoFace.Double(1.0)), 1.0)
  property("Safe Double, Unsafe Double") = verifyTFDouble(min(TwoFace.Double(2.0), TwoFace.Double(us(1.0))), us(1.0))
  property("Unsafe Double, Safe Double") = verifyTFDouble(min(TwoFace.Double(us(2.0)), TwoFace.Double(1.0)), us(1.0))
  property("Unsafe Double, Unsafe Double") = verifyTFDouble(min(TwoFace.Double(us(2.0)), TwoFace.Double(us(1.0))), us(1.0))

  property("Safe Double, Safe Double") = verifyTFDouble(max(TwoFace.Double(2.0), TwoFace.Double(1.0)), 2.0)
  property("Safe Double, Unsafe Double") = verifyTFDouble(max(TwoFace.Double(2.0), TwoFace.Double(us(1.0))), us(2.0))
  property("Unsafe Double, Safe Double") = verifyTFDouble(max(TwoFace.Double(us(2.0)), TwoFace.Double(1.0)), us(2.0))
  property("Unsafe Double, Unsafe Double") = verifyTFDouble(max(TwoFace.Double(us(2.0)), TwoFace.Double(us(1.0))), us(2.0))

  property("Safe Negate") = verifyTFDouble(-TwoFace.Double(-1.0), 1.0)
  property("Unsafe Negate") = verifyTFDouble(-TwoFace.Double(us(1.0)), us(-1.0))

  property("Safe toNat") = wellTyped {
    val nat = TwoFace.Double(2.0).toNat
    verifyOp[nat.N, shapeless.Nat._2]
  }
  property("Safe toChar") = verifyTFChar(TwoFace.Double(1.0).toChar, '\u0001')
  property("Unsafe toChar") = verifyTFChar(TwoFace.Double(us(1.0)).toChar, us('\u0001'))
  property("Safe toInt") = verifyTFInt(TwoFace.Double(1.0).toInt, 1)
  property("Unsafe toInt") = verifyTFInt(TwoFace.Double(us(1.0)).toInt, us(1))
  property("Safe toLong") = verifyTFLong(TwoFace.Double(1.0).toLong, 1L)
  property("Unsafe toLong") = verifyTFLong(TwoFace.Double(us(1.0)).toLong, us(1L))
  property("Safe toFloat") = verifyTFFloat(TwoFace.Double(1.0).toFloat, 1.0f)
  property("Unsafe toFloat") = verifyTFFloat(TwoFace.Double(us(1.0)).toFloat, us(1.0f))
  property("Safe toStringTF") = verifyTFString(TwoFace.Double(1.0).toStringTF, "1.0")
  property("Unsafe toStringTF") = verifyTFString(TwoFace.Double(us(1.5)).toStringTF, us("1.5"))
  property("Safe toSymbol") = {
    val sym = TwoFace.Double(2.0).toSymbol
    sym == scala.Symbol("2.0")
  }

  property("Safe abs") = verifyTFDouble(abs(TwoFace.Double(-1.0)), 1.0)
  property("Unsafe abs") = verifyTFDouble(abs(TwoFace.Double(us(-1.0))), us(1.0))

  property("Safe sin") = verifyTFDouble(sin(TwoFace.Double(1.0)), 0.8414709848078965)
  property("Unsafe sin") = verifyTFDouble(sin(TwoFace.Double(us(1.0))), us(0.8414709848078965))
  property("Safe cos") = verifyTFDouble(cos(TwoFace.Double(1.0)), 0.5403023058681398)
  property("Unsafe cos") = verifyTFDouble(cos(TwoFace.Double(us(1.0))), us(0.5403023058681398))
  property("Safe tan") = verifyTFDouble(tan(TwoFace.Double(1.0)), 1.5574077246549023)
  property("Unsafe tan") = verifyTFDouble(tan(TwoFace.Double(us(1.0))), us(1.5574077246549023))
  property("Safe ceil") = verifyTFDouble(ceil(TwoFace.Double(1.5)), 2.0)
  property("Unsafe ceil") = verifyTFDouble(ceil(TwoFace.Double(us(1.5))), us(2.0))
  property("Safe floor") = verifyTFDouble(floor(TwoFace.Double(1.5)), 1.0)
  property("Unsafe floor") = verifyTFDouble(floor(TwoFace.Double(us(1.5))), us(1.0))
  property("Safe round") = verifyTFLong(round(TwoFace.Double(1.5)), 2L)
  property("Unsafe round") = verifyTFLong(round(TwoFace.Double(us(1.5))), us(2L))
  property("Safe sqrt") = verifyTFDouble(sqrt(TwoFace.Double(9.0)), 3.0)
  property("Unsafe sqrt") = verifyTFDouble(sqrt(TwoFace.Double(us(9.0))), us(3.0))
  property("Safe log") = verifyTFDouble(log(TwoFace.Double(9.0)), 2.1972245773362196)
  property("Unsafe log") = verifyTFDouble(log(TwoFace.Double(us(9.0))), us(2.1972245773362196))
  property("Safe log10") = verifyTFDouble(log10(TwoFace.Double(9.0)), 0.9542425094393249)
  property("Unsafe log10") = verifyTFDouble(log10(TwoFace.Double(us(9.0))), us(0.9542425094393249))

  property("Safe Double pow Safe Float") = verifyTFDouble(pow(TwoFace.Double(2.0), TwoFace.Float(3.0f)), 8.0)
  property("Safe Double pow Unsafe Float") = verifyTFDouble(pow(TwoFace.Double(2.0), TwoFace.Float(us(3.0f))), us(8.0))
  property("Unsafe Double pow Safe Float") = verifyTFDouble(pow(TwoFace.Double(us(2.0)), TwoFace.Float(3.0f)), us(8.0))
  property("Unsafe Double pow Unsafe Float") = verifyTFDouble(pow(TwoFace.Double(us(2.0)), TwoFace.Float(us(3.0f))), us(8.0))
  property("Safe Double pow Safe Double") = verifyTFDouble(pow(TwoFace.Double(2.0), TwoFace.Double(3.0)), 8.0)
  property("Safe Double pow Unsafe Double") = verifyTFDouble(pow(TwoFace.Double(2.0), TwoFace.Double(us(3.0))), us(8.0))
  property("Unsafe Double pow Safe Double") = verifyTFDouble(pow(TwoFace.Double(us(2.0)), TwoFace.Double(3.0)), us(8.0))
  property("Unsafe Double pow Unsafe Double") = verifyTFDouble(pow(TwoFace.Double(us(2.0)), TwoFace.Double(us(3.0))), us(8.0))

  property("Implicit Conversions") = wellTyped {
    val a : TwoFace.Double[W.`3.0`.T] = implicitly[TwoFace.Double[W.`2.0`.T + W.`1.0`.T]]
    val b : TwoFace.Double[W.`3.0`.T + W.`0.0`.T] = implicitly[TwoFace.Double[W.`2.0`.T + W.`1.0`.T]]
    val c : TwoFace.Double[W.`3.0`.T + W.`0.0`.T] = implicitly[TwoFace.Double[W.`3.0`.T]]
    val d : W.`3.0`.T = TwoFace.Double(3.0)
    val e : Double = TwoFace.Double(us(3.0))
    val f : TwoFace.Double[Double] = 3.0
  }

  property("Wrong Implicit Conversions") = wellTyped {
    illTyped("""val impl = implicitly[TwoFace.Double[W.`2.0`.T + W.`2.0`.T]]; val a : TwoFace.Double[W.`3.0`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.Double[W.`2.0`.T + W.`2.0`.T]]; val b : TwoFace.Double[W.`3.0`.T + W.`0.0`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.Double[W.`4.0`.T]]; val c : TwoFace.Double[W.`3.0`.T + W.`0.0`.T] = impl""")
  }

  property("ToString") = {
    TwoFace.Double[W.`1.5`.T].toString() == "1.5"
  }

  type Fin = W.`3.0`.T
  final val fin = 3.0

  property("Extracting from an Upper Bounded Numeric") = wellTyped {
    def foo[W](width: TwoFace.Double[W]) = width
    def foo2[R <: Double](r: R) = foo(r)
    val a = foo2(W(fin).value)
    implicitly[a.Out =:= Fin]
    val b = foo2(us(fin))
    implicitly[b.Out =:= Double]
  }

  property("Extracting from Safe TwoFace") = {
    val a = me(TwoFace.Double(fin))
    val ret = shapeless.the[Id[a.T]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  def noImplFoo[W](w : TwoFace.Double[W]) = -w //Missing twoface shell implicit
  property("Unavailable Implicit Safe TwoFace Shell") = {
    val ret = noImplFoo(2.0)
    implicitly[ret.Out <:< Negate[W.`2.0`.T]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< W.`-2.0`.T]
    retSimple.getValue == -2.0
  }
  property("Unavailable Implicit Unsafe TwoFace Shell") = {
    val ret = noImplFoo(us(2.0))
    implicitly[ret.Out <:< Negate[Double]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< Double]
    retSimple.getValue == -2.0
  }
}
