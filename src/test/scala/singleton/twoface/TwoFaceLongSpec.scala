package singleton.twoface

import singleton.twoface.math._
import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._
import singleton.ops._

class TwoFaceLongSpec extends Properties("TwoFace.Long") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.Long[W.`2L`.T]]
    a.getValue == 2L && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.Long[W.`2L`.T]
    a.getValue == 2L && a.isLiteral
  }
  property("Safe Creation()") = {
    val a = TwoFace.Long(2L)
    a.getValue == 2L && a.isLiteral
  }
  property("Unsafe Creation()") = {
    val a = TwoFace.Long(us(2L))
    a.getValue == 2L && !a.isLiteral
  }

  property("Safe ifThenElse") = verifyTFLong(ifThenElse(true, 1L, 2L), 1L)
  property("Unsafe ifThenElse") = verifyTFLong(ifThenElse(us(false), 1L, 2L), us(2L))

  property("Safe Long + Safe Char") = verifyTFLong(TwoFace.Long(2L) + TwoFace.Char('\u0001'), 3L)
  property("Safe Long + Unsafe Char") = verifyTFLong(TwoFace.Long(2L) + TwoFace.Char(us('\u0001')), us(3L))
  property("Unsafe Long + Safe Char") = verifyTFLong(TwoFace.Long(us(2L)) + TwoFace.Char('\u0001'), us(3L))
  property("Unsafe Long + Unsafe Char") = verifyTFLong(TwoFace.Long(us(2L)) + TwoFace.Char(us('\u0001')), us(3L))
  property("Safe Long + Safe Int") = verifyTFLong(TwoFace.Long(2L) + TwoFace.Int(1), 3L)
  property("Safe Long + Unsafe Int") = verifyTFLong(TwoFace.Long(2L) + TwoFace.Int(us(1)), us(3L))
  property("Unsafe Long + Safe Int") = verifyTFLong(TwoFace.Long(us(2L)) + TwoFace.Int(1), us(3L))
  property("Unsafe Long + Unsafe Int") = verifyTFLong(TwoFace.Long(us(2L)) + TwoFace.Int(us(1)), us(3L))
  property("Safe Long + Safe Long") = verifyTFLong(TwoFace.Long(2L) + TwoFace.Long(1L), 3L)
  property("Safe Long + Unsafe Long") = verifyTFLong(TwoFace.Long(2L) + TwoFace.Long(us(1L)), us(3L))
  property("Unsafe Long + Safe Long") = verifyTFLong(TwoFace.Long(us(2L)) + TwoFace.Long(1L), us(3L))
  property("Unsafe Long + Unsafe Long") = verifyTFLong(TwoFace.Long(us(2L)) + TwoFace.Long(us(1L)), us(3L))
  property("Safe Long + Safe Float") = verifyTFFloat(TwoFace.Long(2L) + TwoFace.Float(1.0f), 3.0f)
  property("Safe Long + Unsafe Float") = verifyTFFloat(TwoFace.Long(2L) + TwoFace.Float(us(1.0f)), us(3.0f))
  property("Unsafe Long + Safe Float") = verifyTFFloat(TwoFace.Long(us(2L)) + TwoFace.Float(1.0f), us(3.0f))
  property("Unsafe Long + Unsafe Float") = verifyTFFloat(TwoFace.Long(us(2L)) + TwoFace.Float(us(1.0f)), us(3.0f))
  property("Safe Long + Safe Double") = verifyTFDouble(TwoFace.Long(2L) + TwoFace.Double(1.0), 3.0)
  property("Safe Long + Unsafe Double") = verifyTFDouble(TwoFace.Long(2L) + TwoFace.Double(us(1.0)), us(3.0))
  property("Unsafe Long + Safe Double") = verifyTFDouble(TwoFace.Long(us(2L)) + TwoFace.Double(1.0), us(3.0))
  property("Unsafe Long + Unsafe Double") = verifyTFDouble(TwoFace.Long(us(2L)) + TwoFace.Double(us(1.0)), us(3.0))

  property("Safe Long - Safe Char") = verifyTFLong(TwoFace.Long(2L) - TwoFace.Char('\u0001'), 1L)
  property("Safe Long - Unsafe Char") = verifyTFLong(TwoFace.Long(2L) - TwoFace.Char(us('\u0001')), us(1L))
  property("Unsafe Long - Safe Char") = verifyTFLong(TwoFace.Long(us(2L)) - TwoFace.Char('\u0001'), us(1L))
  property("Unsafe Long - Unsafe Char") = verifyTFLong(TwoFace.Long(us(2L)) - TwoFace.Char(us('\u0001')), us(1L))
  property("Safe Long - Safe Int") = verifyTFLong(TwoFace.Long(2L) - TwoFace.Int(1), 1L)
  property("Safe Long - Unsafe Int") = verifyTFLong(TwoFace.Long(2L) - TwoFace.Int(us(1)), us(1L))
  property("Unsafe Long - Safe Int") = verifyTFLong(TwoFace.Long(us(2L)) - TwoFace.Int(1), us(1L))
  property("Unsafe Long - Unsafe Int") = verifyTFLong(TwoFace.Long(us(2L)) - TwoFace.Int(us(1)), us(1L))
  property("Safe Long - Safe Long") = verifyTFLong(TwoFace.Long(2L) - TwoFace.Long(1L), 1L)
  property("Safe Long - Unsafe Long") = verifyTFLong(TwoFace.Long(2L) - TwoFace.Long(us(1L)), us(1L))
  property("Unsafe Long - Safe Long") = verifyTFLong(TwoFace.Long(us(2L)) - TwoFace.Long(1L), us(1L))
  property("Unsafe Long - Unsafe Long") = verifyTFLong(TwoFace.Long(us(2L)) - TwoFace.Long(us(1L)), us(1L))
  property("Safe Long - Safe Float") = verifyTFFloat(TwoFace.Long(2L) - TwoFace.Float(1.0f), 1.0f)
  property("Safe Long - Unsafe Float") = verifyTFFloat(TwoFace.Long(2L) - TwoFace.Float(us(1.0f)), us(1.0f))
  property("Unsafe Long - Safe Float") = verifyTFFloat(TwoFace.Long(us(2L)) - TwoFace.Float(1.0f), us(1.0f))
  property("Unsafe Long - Unsafe Float") = verifyTFFloat(TwoFace.Long(us(2L)) - TwoFace.Float(us(1.0f)), us(1.0f))
  property("Safe Long - Safe Double") = verifyTFDouble(TwoFace.Long(2L) - TwoFace.Double(1.0), 1.0)
  property("Safe Long - Unsafe Double") = verifyTFDouble(TwoFace.Long(2L) - TwoFace.Double(us(1.0)), us(1.0))
  property("Unsafe Long - Safe Double") = verifyTFDouble(TwoFace.Long(us(2L)) - TwoFace.Double(1.0), us(1.0))
  property("Unsafe Long - Unsafe Double") = verifyTFDouble(TwoFace.Long(us(2L)) - TwoFace.Double(us(1.0)), us(1.0))

  property("Safe Long * Safe Char") = verifyTFLong(TwoFace.Long(2L) * TwoFace.Char('\u0001'), 2L)
  property("Safe Long * Unsafe Char") = verifyTFLong(TwoFace.Long(2L) * TwoFace.Char(us('\u0001')), us(2L))
  property("Unsafe Long * Safe Char") = verifyTFLong(TwoFace.Long(us(2L)) * TwoFace.Char('\u0001'), us(2L))
  property("Unsafe Long * Unsafe Char") = verifyTFLong(TwoFace.Long(us(2L)) * TwoFace.Char(us('\u0001')), us(2L))
  property("Safe Long * Safe Int") = verifyTFLong(TwoFace.Long(2L) * TwoFace.Int(1), 2L)
  property("Safe Long * Unsafe Int") = verifyTFLong(TwoFace.Long(2L) * TwoFace.Int(us(1)), us(2L))
  property("Unsafe Long * Safe Int") = verifyTFLong(TwoFace.Long(us(2L)) * TwoFace.Int(1), us(2L))
  property("Unsafe Long * Unsafe Int") = verifyTFLong(TwoFace.Long(us(2L)) * TwoFace.Int(us(1)), us(2L))
  property("Safe Long * Safe Long") = verifyTFLong(TwoFace.Long(2L) * TwoFace.Long(1L), 2L)
  property("Safe Long * Unsafe Long") = verifyTFLong(TwoFace.Long(2L) * TwoFace.Long(us(1L)), us(2L))
  property("Unsafe Long * Safe Long") = verifyTFLong(TwoFace.Long(us(2L)) * TwoFace.Long(1L), us(2L))
  property("Unsafe Long * Unsafe Long") = verifyTFLong(TwoFace.Long(us(2L)) * TwoFace.Long(us(1L)), us(2L))
  property("Safe Long * Safe Float") = verifyTFFloat(TwoFace.Long(2L) * TwoFace.Float(1.0f), 2.0f)
  property("Safe Long * Unsafe Float") = verifyTFFloat(TwoFace.Long(2L) * TwoFace.Float(us(1.0f)), us(2.0f))
  property("Unsafe Long * Safe Float") = verifyTFFloat(TwoFace.Long(us(2L)) * TwoFace.Float(1.0f), us(2.0f))
  property("Unsafe Long * Unsafe Float") = verifyTFFloat(TwoFace.Long(us(2L)) * TwoFace.Float(us(1.0f)), us(2.0f))
  property("Safe Long * Safe Double") = verifyTFDouble(TwoFace.Long(2L) * TwoFace.Double(1.0), 2.0)
  property("Safe Long * Unsafe Double") = verifyTFDouble(TwoFace.Long(2L) * TwoFace.Double(us(1.0)), us(2.0))
  property("Unsafe Long * Safe Double") = verifyTFDouble(TwoFace.Long(us(2L)) * TwoFace.Double(1.0), us(2.0))
  property("Unsafe Long * Unsafe Double") = verifyTFDouble(TwoFace.Long(us(2L)) * TwoFace.Double(us(1.0)), us(2.0))

  property("Safe Long / Safe Char") = verifyTFLong(TwoFace.Long(6L) / TwoFace.Char('\u0002'), 3L)
  property("Safe Long / Unsafe Char") = verifyTFLong(TwoFace.Long(6L) / TwoFace.Char(us('\u0002')), us(3L))
  property("Unsafe Long / Safe Char") = verifyTFLong(TwoFace.Long(us(6L)) / TwoFace.Char('\u0002'), us(3L))
  property("Unsafe Long / Unsafe Char") = verifyTFLong(TwoFace.Long(us(6L)) / TwoFace.Char(us('\u0002')), us(3L))
  property("Safe Long / Safe Int") = verifyTFLong(TwoFace.Long(6L) / TwoFace.Int(2), 3L)
  property("Safe Long / Unsafe Int") = verifyTFLong(TwoFace.Long(6L) / TwoFace.Int(us(2)), us(3L))
  property("Unsafe Long / Safe Int") = verifyTFLong(TwoFace.Long(us(6L)) / TwoFace.Int(2), us(3L))
  property("Unsafe Long / Unsafe Int") = verifyTFLong(TwoFace.Long(us(6L)) / TwoFace.Int(us(2)), us(3L))
  property("Safe Long / Safe Long") = verifyTFLong(TwoFace.Long(6L) / TwoFace.Long(2L), 3L)
  property("Safe Long / Unsafe Long") = verifyTFLong(TwoFace.Long(6L) / TwoFace.Long(us(2L)), us(3L))
  property("Unsafe Long / Safe Long") = verifyTFLong(TwoFace.Long(us(6L)) / TwoFace.Long(2L), us(3L))
  property("Unsafe Long / Unsafe Long") = verifyTFLong(TwoFace.Long(us(6L)) / TwoFace.Long(us(2L)), us(3L))
  property("Safe Long / Safe Float") = verifyTFFloat(TwoFace.Long(6L) / TwoFace.Float(2.0f), 3.0f)
  property("Safe Long / Unsafe Float") = verifyTFFloat(TwoFace.Long(6L) / TwoFace.Float(us(2.0f)), us(3.0f))
  property("Unsafe Long / Safe Float") = verifyTFFloat(TwoFace.Long(us(6L)) / TwoFace.Float(2.0f), us(3.0f))
  property("Unsafe Long / Unsafe Float") = verifyTFFloat(TwoFace.Long(us(6L)) / TwoFace.Float(us(2.0f)), us(3.0f))
  property("Safe Long / Safe Double") = verifyTFDouble(TwoFace.Long(6L) / TwoFace.Double(2.0), 3.0)
  property("Safe Long / Unsafe Double") = verifyTFDouble(TwoFace.Long(6L) / TwoFace.Double(us(2.0)), us(3.0))
  property("Unsafe Long / Safe Double") = verifyTFDouble(TwoFace.Long(us(6L)) / TwoFace.Double(2.0), us(3.0))
  property("Unsafe Long / Unsafe Double") = verifyTFDouble(TwoFace.Long(us(6L)) / TwoFace.Double(us(2.0)), us(3.0))

  property("Safe Long % Safe Char") = verifyTFLong(TwoFace.Long(7L) % TwoFace.Char('\u0004'), 3L)
  property("Safe Long % Unsafe Char") = verifyTFLong(TwoFace.Long(7L) % TwoFace.Char(us('\u0004')), us(3L))
  property("Unsafe Long % Safe Char") = verifyTFLong(TwoFace.Long(us(7L)) % TwoFace.Char('\u0004'), us(3L))
  property("Unsafe Long % Unsafe Char") = verifyTFLong(TwoFace.Long(us(7L)) % TwoFace.Char(us('\u0004')), us(3L))
  property("Safe Long % Safe Int") = verifyTFLong(TwoFace.Long(7L) % TwoFace.Int(4), 3L)
  property("Safe Long % Unsafe Int") = verifyTFLong(TwoFace.Long(7L) % TwoFace.Int(us(4)), us(3L))
  property("Unsafe Long % Safe Int") = verifyTFLong(TwoFace.Long(us(7L)) % TwoFace.Int(4), us(3L))
  property("Unsafe Long % Unsafe Int") = verifyTFLong(TwoFace.Long(us(7L)) % TwoFace.Int(us(4)), us(3L))
  property("Safe Long % Safe Long") = verifyTFLong(TwoFace.Long(7L) % TwoFace.Long(4L), 3L)
  property("Safe Long % Unsafe Long") = verifyTFLong(TwoFace.Long(7L) % TwoFace.Long(us(4L)), us(3L))
  property("Unsafe Long % Safe Long") = verifyTFLong(TwoFace.Long(us(7L)) % TwoFace.Long(4L), us(3L))
  property("Unsafe Long % Unsafe Long") = verifyTFLong(TwoFace.Long(us(7L)) % TwoFace.Long(us(4L)), us(3L))
  property("Safe Long % Safe Float") = verifyTFFloat(TwoFace.Long(7L) % TwoFace.Float(4.0f), 3.0f)
  property("Safe Long % Unsafe Float") = verifyTFFloat(TwoFace.Long(7L) % TwoFace.Float(us(4.0f)), us(3.0f))
  property("Unsafe Long % Safe Float") = verifyTFFloat(TwoFace.Long(us(7L)) % TwoFace.Float(4.0f), us(3.0f))
  property("Unsafe Long % Unsafe Float") = verifyTFFloat(TwoFace.Long(us(7L)) % TwoFace.Float(us(4.0f)), us(3.0f))
  property("Safe Long % Safe Double") = verifyTFDouble(TwoFace.Long(7L) % TwoFace.Double(4.0), 3.0)
  property("Safe Long % Unsafe Double") = verifyTFDouble(TwoFace.Long(7L) % TwoFace.Double(us(4.0)), us(3.0))
  property("Unsafe Long % Safe Double") = verifyTFDouble(TwoFace.Long(us(7L)) % TwoFace.Double(4.0), us(3.0))
  property("Unsafe Long % Unsafe Double") = verifyTFDouble(TwoFace.Long(us(7L)) % TwoFace.Double(us(4.0)), us(3.0))

  property("Safe Long < Safe Char") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Char('\u0004'), false)
  property("Safe Long < Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Long < Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Char('\u0004'), us(false))
  property("Unsafe Long < Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Char(us('\u0004')), us(false))
  property("Safe Long < Safe Int") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Int(4), false)
  property("Safe Long < Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Int(us(4)), us(false))
  property("Unsafe Long < Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Int(4), us(false))
  property("Unsafe Long < Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Int(us(4)), us(false))
  property("Safe Long < Safe Long") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Long(4L), false)
  property("Safe Long < Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Long(us(4L)), us(false))
  property("Unsafe Long < Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Long(4L), us(false))
  property("Unsafe Long < Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Long(us(4L)), us(false))
  property("Safe Long < Safe Float") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Float(4.0f), false)
  property("Safe Long < Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Long < Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Float(4.0f), us(false))
  property("Unsafe Long < Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Float(us(4.0f)), us(false))
  property("Safe Long < Safe Double") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Double(4.0), false)
  property("Safe Long < Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) < TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Long < Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Double(4.0), us(false))
  property("Unsafe Long < Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) < TwoFace.Double(us(4.0)), us(false))

  property("Safe Long > Safe Char") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Char('\u0004'), true)
  property("Safe Long > Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Long > Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Char('\u0004'), us(true))
  property("Unsafe Long > Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Char(us('\u0004')), us(true))
  property("Safe Long > Safe Int") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Int(4), true)
  property("Safe Long > Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Int(us(4)), us(true))
  property("Unsafe Long > Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Int(4), us(true))
  property("Unsafe Long > Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Int(us(4)), us(true))
  property("Safe Long > Safe Long") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Long(4L), true)
  property("Safe Long > Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Long(us(4L)), us(true))
  property("Unsafe Long > Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Long(4L), us(true))
  property("Unsafe Long > Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Long(us(4L)), us(true))
  property("Safe Long > Safe Float") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Float(4.0f), true)
  property("Safe Long > Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Long > Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Float(4.0f), us(true))
  property("Unsafe Long > Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Float(us(4.0f)), us(true))
  property("Safe Long > Safe Double") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Double(4.0), true)
  property("Safe Long > Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) > TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Long > Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Double(4.0), us(true))
  property("Unsafe Long > Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) > TwoFace.Double(us(4.0)), us(true))

  property("Safe Long <= Safe Char") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Char('\u0004'), false)
  property("Safe Long <= Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Long <= Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Char('\u0004'), us(false))
  property("Unsafe Long <= Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Char(us('\u0004')), us(false))
  property("Safe Long <= Safe Int") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Int(4), false)
  property("Safe Long <= Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Int(us(4)), us(false))
  property("Unsafe Long <= Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Int(4), us(false))
  property("Unsafe Long <= Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Int(us(4)), us(false))
  property("Safe Long <= Safe Long") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Long(4L), false)
  property("Safe Long <= Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Long(us(4L)), us(false))
  property("Unsafe Long <= Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Long(4L), us(false))
  property("Unsafe Long <= Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Long(us(4L)), us(false))
  property("Safe Long <= Safe Float") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Float(4.0f), false)
  property("Safe Long <= Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Long <= Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Float(4.0f), us(false))
  property("Unsafe Long <= Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Float(us(4.0f)), us(false))
  property("Safe Long <= Safe Double") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Double(4.0), false)
  property("Safe Long <= Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) <= TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Long <= Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Double(4.0), us(false))
  property("Unsafe Long <= Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) <= TwoFace.Double(us(4.0)), us(false))

  property("Safe Long >= Safe Char") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Char('\u0004'), true)
  property("Safe Long >= Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Long >= Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Char('\u0004'), us(true))
  property("Unsafe Long >= Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Char(us('\u0004')), us(true))
  property("Safe Long >= Safe Int") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Int(4), true)
  property("Safe Long >= Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Int(us(4)), us(true))
  property("Unsafe Long >= Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Int(4), us(true))
  property("Unsafe Long >= Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Int(us(4)), us(true))
  property("Safe Long >= Safe Long") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Long(4L), true)
  property("Safe Long >= Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Long(us(4L)), us(true))
  property("Unsafe Long >= Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Long(4L), us(true))
  property("Unsafe Long >= Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Long(us(4L)), us(true))
  property("Safe Long >= Safe Float") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Float(4.0f), true)
  property("Safe Long >= Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Long >= Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Float(4.0f), us(true))
  property("Unsafe Long >= Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Float(us(4.0f)), us(true))
  property("Safe Long >= Safe Double") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Double(4.0), true)
  property("Safe Long >= Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) >= TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Long >= Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Double(4.0), us(true))
  property("Unsafe Long >= Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) >= TwoFace.Double(us(4.0)), us(true))

  property("Safe Long == Regular Safe Char") = verifyTFBoolean(TwoFace.Long(7L) == ('\u0007'), true)
  property("Safe Long == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) == (us('\u0007')), us(true))
  property("Unsafe Long == Regular Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) == ('\u0007'), us(true))
  property("Unsafe Long == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) == (us('\u0007')), us(true))
  property("Safe Long == Regular Safe Int") = verifyTFBoolean(TwoFace.Long(7L) == (7), true)
  property("Safe Long == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) == (us(7)), us(true))
  property("Unsafe Long == Regular Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) == (7), us(true))
  property("Unsafe Long == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) == (us(7)), us(true))
  property("Safe Long == Regular Safe Long") = verifyTFBoolean(TwoFace.Long(7L) == (7L), true)
  property("Safe Long == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) == (us(7L)), us(true))
  property("Unsafe Long == Regular Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) == (7L), us(true))
  property("Unsafe Long == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) == (us(7L)), us(true))
  property("Safe Long == Regular Safe Float") = verifyTFBoolean(TwoFace.Long(7L) == (7.0f), true)
  property("Safe Long == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) == (us(7.0f)), us(true))
  property("Unsafe Long == Regular Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) == (7.0f), us(true))
  property("Unsafe Long == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) == (us(7.0f)), us(true))
  property("Safe Long == Regular Safe Double") = verifyTFBoolean(TwoFace.Long(7L) == (7.0), true)
  property("Safe Long == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) == (us(7.0)), us(true))
  property("Unsafe Long == Regular Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) == (7.0), us(true))
  property("Unsafe Long == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) == (us(7.0)), us(true))

  property("Safe Long != Regular Safe Char") = verifyTFBoolean(TwoFace.Long(7L) != ('\u0007'), false)
  property("Safe Long != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) != (us('\u0007')), us(false))
  property("Unsafe Long != Regular Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) != ('\u0007'), us(false))
  property("Unsafe Long != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) != (us('\u0007')), us(false))
  property("Safe Long != Regular Safe Int") = verifyTFBoolean(TwoFace.Long(7L) != (7), false)
  property("Safe Long != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) != (us(7)), us(false))
  property("Unsafe Long != Regular Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) != (7), us(false))
  property("Unsafe Long != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) != (us(7)), us(false))
  property("Safe Long != Regular Safe Long") = verifyTFBoolean(TwoFace.Long(7L) != (7L), false)
  property("Safe Long != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) != (us(7L)), us(false))
  property("Unsafe Long != Regular Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) != (7L), us(false))
  property("Unsafe Long != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) != (us(7L)), us(false))
  property("Safe Long != Regular Safe Float") = verifyTFBoolean(TwoFace.Long(7L) != (7.0f), false)
  property("Safe Long != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) != (us(7.0f)), us(false))
  property("Unsafe Long != Regular Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) != (7.0f), us(false))
  property("Unsafe Long != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) != (us(7.0f)), us(false))
  property("Safe Long != Regular Safe Double") = verifyTFBoolean(TwoFace.Long(7L) != (7.0), false)
  property("Safe Long != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) != (us(7.0)), us(false))
  property("Unsafe Long != Regular Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) != (7.0), us(false))
  property("Unsafe Long != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) != (us(7.0)), us(false))

  property("Safe Long == Safe Char") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Char('\u0007'), true)
  property("Safe Long == Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Char(us('\u0007')), us(true))
  property("Unsafe Long == Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Char('\u0007'), us(true))
  property("Unsafe Long == Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Char(us('\u0007')), us(true))
  property("Safe Long == Safe Int") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Int(7), true)
  property("Safe Long == Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Int(us(7)), us(true))
  property("Unsafe Long == Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Int(7), us(true))
  property("Unsafe Long == Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Int(us(7)), us(true))
  property("Safe Long == Safe Long") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Long(7L), true)
  property("Safe Long == Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Long(us(7L)), us(true))
  property("Unsafe Long == Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Long(7L), us(true))
  property("Unsafe Long == Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Long(us(7L)), us(true))
  property("Safe Long == Safe Float") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Float(7.0f), true)
  property("Safe Long == Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Float(us(7.0f)), us(true))
  property("Unsafe Long == Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Float(7.0f), us(true))
  property("Unsafe Long == Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Float(us(7.0f)), us(true))
  property("Safe Long == Safe Double") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Double(7.0), true)
  property("Safe Long == Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) == TwoFace.Double(us(7.0)), us(true))
  property("Unsafe Long == Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Double(7.0), us(true))
  property("Unsafe Long == Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) == TwoFace.Double(us(7.0)), us(true))

  property("Safe Long != Safe Char") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Char('\u0007'), false)
  property("Safe Long != Unsafe Char") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Char(us('\u0007')), us(false))
  property("Unsafe Long != Safe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Char('\u0007'), us(false))
  property("Unsafe Long != Unsafe Char") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Char(us('\u0007')), us(false))
  property("Safe Long != Safe Int") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Int(7), false)
  property("Safe Long != Unsafe Int") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Int(us(7)), us(false))
  property("Unsafe Long != Safe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Int(7), us(false))
  property("Unsafe Long != Unsafe Int") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Int(us(7)), us(false))
  property("Safe Long != Safe Long") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Long(7L), false)
  property("Safe Long != Unsafe Long") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Long(us(7L)), us(false))
  property("Unsafe Long != Safe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Long(7L), us(false))
  property("Unsafe Long != Unsafe Long") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Long(us(7L)), us(false))
  property("Safe Long != Safe Float") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Float(7.0f), false)
  property("Safe Long != Unsafe Float") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Float(us(7.0f)), us(false))
  property("Unsafe Long != Safe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Float(7.0f), us(false))
  property("Unsafe Long != Unsafe Float") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Float(us(7.0f)), us(false))
  property("Safe Long != Safe Double") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Double(7.0), false)
  property("Safe Long != Unsafe Double") = verifyTFBoolean(TwoFace.Long(7L) != TwoFace.Double(us(7.0)), us(false))
  property("Unsafe Long != Safe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Double(7.0), us(false))
  property("Unsafe Long != Unsafe Double") = verifyTFBoolean(TwoFace.Long(us(7L)) != TwoFace.Double(us(7.0)), us(false))

  property("Safe Long, Safe Long") = verifyTFLong(min(TwoFace.Long(2L), TwoFace.Long(1L)), 1L)
  property("Safe Long, Unsafe Long") = verifyTFLong(min(TwoFace.Long(2L), TwoFace.Long(us(1L))), us(1L))
  property("Unsafe Long, Safe Long") = verifyTFLong(min(TwoFace.Long(us(2L)), TwoFace.Long(1L)), us(1L))
  property("Unsafe Long, Unsafe Long") = verifyTFLong(min(TwoFace.Long(us(2L)), TwoFace.Long(us(1L))), us(1L))

  property("Safe Long, Safe Long") = verifyTFLong(max(TwoFace.Long(2L), TwoFace.Long(1L)), 2L)
  property("Safe Long, Unsafe Long") = verifyTFLong(max(TwoFace.Long(2L), TwoFace.Long(us(1L))), us(2L))
  property("Unsafe Long, Safe Long") = verifyTFLong(max(TwoFace.Long(us(2L)), TwoFace.Long(1L)), us(2L))
  property("Unsafe Long, Unsafe Long") = verifyTFLong(max(TwoFace.Long(us(2L)), TwoFace.Long(us(1L))), us(2L))

  property("Safe Negate") = verifyTFLong(-TwoFace.Long(-1L), 1L)
  property("Unsafe Negate") = verifyTFLong(-TwoFace.Long(us(1L)), us(-1L))

  property("Safe toNat") = wellTyped {
    val nat = TwoFace.Long(3L).toNat
    verifyOp[nat.N, shapeless.Nat._3]
  }
  property("Safe toChar") = verifyTFChar(TwoFace.Long(1L).toChar, '\u0001')
  property("Unsafe toChar") = verifyTFChar(TwoFace.Long(us(1L)).toChar, us('\u0001'))
  property("Safe toInt") = verifyTFInt(TwoFace.Long(1L).toInt, 1)
  property("Unsafe toInt") = verifyTFInt(TwoFace.Long(us(1L)).toInt, us(1))
  property("Safe toFloat") = verifyTFFloat(TwoFace.Long(1L).toFloat, 1.0f)
  property("Unsafe toFloat") = verifyTFFloat(TwoFace.Long(us(1L)).toFloat, us(1.0f))
  property("Safe toDouble") = verifyTFDouble(TwoFace.Long(1L).toDouble, 1.0)
  property("Unsafe toDouble") = verifyTFDouble(TwoFace.Long(us(1L)).toDouble, us(1.0))
  property("Safe toStringTF") = verifyTFString(TwoFace.Long(1L).toStringTF, "1")
  property("Unsafe toStringTF") = verifyTFString(TwoFace.Long(us(1L)).toStringTF, us("1"))
  property("Safe toSymbol") = {
    val sym = TwoFace.Long(2L).toSymbol
    sym == scala.Symbol("2")
  }

  property("Safe abs") = verifyTFLong(abs(TwoFace.Long(-1L)), 1L)
  property("Unsafe abs") = verifyTFLong(abs(TwoFace.Long(us(-1L))), us(1L))

  property("Safe numberOfLeadingZeros") = verifyTFInt(TwoFace.Long.numberOfLeadingZeros(TwoFace.Long(1L)), 63)
  property("Unsafe numberOfLeadingZeros") = verifyTFInt(TwoFace.Long.numberOfLeadingZeros(TwoFace.Long(us(1L))), us(63))

  property("Implicit Conversions") = wellTyped {
    val a : TwoFace.Long[W.`3L`.T] = implicitly[TwoFace.Long[W.`2L`.T + W.`1L`.T]]
    val b : TwoFace.Long[W.`3L`.T + W.`0L`.T] = implicitly[TwoFace.Long[W.`2L`.T + W.`1L`.T]]
    val c : TwoFace.Long[W.`3L`.T + W.`0L`.T] = implicitly[TwoFace.Long[W.`3L`.T]]
    val d : W.`3L`.T = TwoFace.Long(3L)
    val e : Long = TwoFace.Long(us(3L))
    val f : TwoFace.Long[Long] = 3L
  }

  property("Wrong Implicit Conversions") = wellTyped {
    illTyped("""val impl = implicitly[TwoFace.Long[W.`2L`.T + W.`2L`.T]]; val a : TwoFace.Long[W.`3L`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.Long[W.`2L`.T + W.`2L`.T]]; val b : TwoFace.Long[W.`3L`.T + W.`0L`.T] = impl""")
    illTyped("""val impl = implicitly[TwoFace.Long[W.`4L`.T]]; val c : TwoFace.Long[W.`3L`.T + W.`0L`.T] = impl""")
  }

  property("ToString") = {
    TwoFace.Long[W.`1L`.T].toString() == "1"
  }

  type Fin = W.`3L`.T
  final val fin = 3L

  property("Extracting from an Upper Bounded Numeric") = wellTyped {
    def foo[W](width: TwoFace.Long[W]) = width
    def foo2[R <: Long](r: R) = foo(r)
    val a = foo2(W(fin).value)
    implicitly[a.Out =:= Fin]
    val b = foo2(us(fin))
    implicitly[b.Out =:= Long]
  }

  property("Extracting from Safe TwoFace") = {
    val a = me(TwoFace.Long(fin))
    val ret = shapeless.the[Id[a.T]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  def noImplFoo[W](w : TwoFace.Long[W]) = -w //Missing twoface shell implicit
  property("Unavailable Implicit Safe TwoFace Shell") = {
    val ret = noImplFoo(2L)
    implicitly[ret.Out <:< Negate[W.`2L`.T]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< W.`-2L`.T]
    retSimple.getValue == -2L
  }
  property("Unavailable Implicit Unsafe TwoFace Shell") = {
    val ret = noImplFoo(us(2L))
    implicitly[ret.Out <:< Negate[Long]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< Long]
    retSimple.getValue == -2L
  }
}