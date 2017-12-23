package singleton.twoface

import org.scalacheck.Properties
import singleton.TestUtils._
import singleton.ops._

class TwoFaceCharSpec extends Properties("TwoFace.Char") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.Char[W.`'\u0002'`.T]]
    a.getValue == '\u0002' && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.Char[W.`'\u0002'`.T]
    a.getValue == '\u0002' && a.isLiteral
  }
  property("Safe Creation()") = {
    val a = TwoFace.Char('\u0002')
    a.getValue == '\u0002' && a.isLiteral
  }
  property("Unsafe Creation()") = {
    val a = TwoFace.Char(us('\u0002'))
    a.getValue == '\u0002' && !a.isLiteral
  }

  property("Safe ifThenElse") = verifyTFChar(ifThenElse(true, '\u0001', '\u0002'), '\u0001')
  property("Unsafe ifThenElse") = verifyTFChar(ifThenElse(us(false), '\u0001', '\u0002'), us('\u0002'))

  property("Safe Char + Safe Char") = verifyTFInt(TwoFace.Char('\u0002') + TwoFace.Char('\u0001'), 3)
  property("Safe Char + Unsafe Char") = verifyTFInt(TwoFace.Char('\u0002') + TwoFace.Char(us('\u0001')), us(3))
  property("Unsafe Char + Safe Char") = verifyTFInt(TwoFace.Char(us('\u0002')) + TwoFace.Char('\u0001'), us(3))
  property("Unsafe Char + Unsafe Char") = verifyTFInt(TwoFace.Char(us('\u0002')) + TwoFace.Char(us('\u0001')), us(3))
  property("Safe Char + Safe Int") = verifyTFInt(TwoFace.Char('\u0002') + TwoFace.Int(1), 3)
  property("Safe Char + Unsafe Int") = verifyTFInt(TwoFace.Char('\u0002') + TwoFace.Int(us(1)), us(3))
  property("Unsafe Char + Safe Int") = verifyTFInt(TwoFace.Char(us('\u0002')) + TwoFace.Int(1), us(3))
  property("Unsafe Char + Unsafe Int") = verifyTFInt(TwoFace.Char(us('\u0002')) + TwoFace.Int(us(1)), us(3))
  property("Safe Char + Safe Long") = verifyTFLong(TwoFace.Char('\u0002') + TwoFace.Long(1L), 3L)
  property("Safe Char + Unsafe Long") = verifyTFLong(TwoFace.Char('\u0002') + TwoFace.Long(us(1L)), us(3L))
  property("Unsafe Char + Safe Long") = verifyTFLong(TwoFace.Char(us('\u0002')) + TwoFace.Long(1L), us(3L))
  property("Unsafe Char + Unsafe Long") = verifyTFLong(TwoFace.Char(us('\u0002')) + TwoFace.Long(us(1L)), us(3L))
  property("Safe Char + Safe Float") = verifyTFFloat(TwoFace.Char('\u0002') + TwoFace.Float(1.0f), 3.0f)
  property("Safe Char + Unsafe Float") = verifyTFFloat(TwoFace.Char('\u0002') + TwoFace.Float(us(1.0f)), us(3.0f))
  property("Unsafe Char + Safe Float") = verifyTFFloat(TwoFace.Char(us('\u0002')) + TwoFace.Float(1.0f), us(3.0f))
  property("Unsafe Char + Unsafe Float") = verifyTFFloat(TwoFace.Char(us('\u0002')) + TwoFace.Float(us(1.0f)), us(3.0f))
  property("Safe Char + Safe Double") = verifyTFDouble(TwoFace.Char('\u0002') + TwoFace.Double(1.0), 3.0)
  property("Safe Char + Unsafe Double") = verifyTFDouble(TwoFace.Char('\u0002') + TwoFace.Double(us(1.0)), us(3.0))
  property("Unsafe Char + Safe Double") = verifyTFDouble(TwoFace.Char(us('\u0002')) + TwoFace.Double(1.0), us(3.0))
  property("Unsafe Char + Unsafe Double") = verifyTFDouble(TwoFace.Char(us('\u0002')) + TwoFace.Double(us(1.0)), us(3.0))

  property("Safe Char - Safe Char") = verifyTFInt(TwoFace.Char('\u0002') - TwoFace.Char('\u0001'), 1)
  property("Safe Char - Unsafe Char") = verifyTFInt(TwoFace.Char('\u0002') - TwoFace.Char(us('\u0001')), us(1))
  property("Unsafe Char - Safe Char") = verifyTFInt(TwoFace.Char(us('\u0002')) - TwoFace.Char('\u0001'), us(1))
  property("Unsafe Char - Unsafe Char") = verifyTFInt(TwoFace.Char(us('\u0002')) - TwoFace.Char(us('\u0001')), us(1))
  property("Safe Char - Safe Int") = verifyTFInt(TwoFace.Char('\u0002') - TwoFace.Int(1), 1)
  property("Safe Char - Unsafe Int") = verifyTFInt(TwoFace.Char('\u0002') - TwoFace.Int(us(1)), us(1))
  property("Unsafe Char - Safe Int") = verifyTFInt(TwoFace.Char(us('\u0002')) - TwoFace.Int(1), us(1))
  property("Unsafe Char - Unsafe Int") = verifyTFInt(TwoFace.Char(us('\u0002')) - TwoFace.Int(us(1)), us(1))
  property("Safe Char - Safe Long") = verifyTFLong(TwoFace.Char('\u0002') - TwoFace.Long(1L), 1L)
  property("Safe Char - Unsafe Long") = verifyTFLong(TwoFace.Char('\u0002') - TwoFace.Long(us(1L)), us(1L))
  property("Unsafe Char - Safe Long") = verifyTFLong(TwoFace.Char(us('\u0002')) - TwoFace.Long(1L), us(1L))
  property("Unsafe Char - Unsafe Long") = verifyTFLong(TwoFace.Char(us('\u0002')) - TwoFace.Long(us(1L)), us(1L))
  property("Safe Char - Safe Float") = verifyTFFloat(TwoFace.Char('\u0002') - TwoFace.Float(1.0f), 1.0f)
  property("Safe Char - Unsafe Float") = verifyTFFloat(TwoFace.Char('\u0002') - TwoFace.Float(us(1.0f)), us(1.0f))
  property("Unsafe Char - Safe Float") = verifyTFFloat(TwoFace.Char(us('\u0002')) - TwoFace.Float(1.0f), us(1.0f))
  property("Unsafe Char - Unsafe Float") = verifyTFFloat(TwoFace.Char(us('\u0002')) - TwoFace.Float(us(1.0f)), us(1.0f))
  property("Safe Char - Safe Double") = verifyTFDouble(TwoFace.Char('\u0002') - TwoFace.Double(1.0), 1.0)
  property("Safe Char - Unsafe Double") = verifyTFDouble(TwoFace.Char('\u0002') - TwoFace.Double(us(1.0)), us(1.0))
  property("Unsafe Char - Safe Double") = verifyTFDouble(TwoFace.Char(us('\u0002')) - TwoFace.Double(1.0), us(1.0))
  property("Unsafe Char - Unsafe Double") = verifyTFDouble(TwoFace.Char(us('\u0002')) - TwoFace.Double(us(1.0)), us(1.0))

  property("Safe Char * Safe Char") = verifyTFInt(TwoFace.Char('\u0002') * TwoFace.Char('\u0001'), 2)
  property("Safe Char * Unsafe Char") = verifyTFInt(TwoFace.Char('\u0002') * TwoFace.Char(us('\u0001')), us(2))
  property("Unsafe Char * Safe Char") = verifyTFInt(TwoFace.Char(us('\u0002')) * TwoFace.Char('\u0001'), us(2))
  property("Unsafe Char * Unsafe Char") = verifyTFInt(TwoFace.Char(us('\u0002')) * TwoFace.Char(us('\u0001')), us(2))
  property("Safe Char * Safe Int") = verifyTFInt(TwoFace.Char('\u0002') * TwoFace.Int(1), 2)
  property("Safe Char * Unsafe Int") = verifyTFInt(TwoFace.Char('\u0002') * TwoFace.Int(us(1)), us(2))
  property("Unsafe Char * Safe Int") = verifyTFInt(TwoFace.Char(us('\u0002')) * TwoFace.Int(1), us(2))
  property("Unsafe Char * Unsafe Int") = verifyTFInt(TwoFace.Char(us('\u0002')) * TwoFace.Int(us(1)), us(2))
  property("Safe Char * Safe Long") = verifyTFLong(TwoFace.Char('\u0002') * TwoFace.Long(1L), 2L)
  property("Safe Char * Unsafe Long") = verifyTFLong(TwoFace.Char('\u0002') * TwoFace.Long(us(1L)), us(2L))
  property("Unsafe Char * Safe Long") = verifyTFLong(TwoFace.Char(us('\u0002')) * TwoFace.Long(1L), us(2L))
  property("Unsafe Char * Unsafe Long") = verifyTFLong(TwoFace.Char(us('\u0002')) * TwoFace.Long(us(1L)), us(2L))
  property("Safe Char * Safe Float") = verifyTFFloat(TwoFace.Char('\u0002') * TwoFace.Float(1.0f), 2.0f)
  property("Safe Char * Unsafe Float") = verifyTFFloat(TwoFace.Char('\u0002') * TwoFace.Float(us(1.0f)), us(2.0f))
  property("Unsafe Char * Safe Float") = verifyTFFloat(TwoFace.Char(us('\u0002')) * TwoFace.Float(1.0f), us(2.0f))
  property("Unsafe Char * Unsafe Float") = verifyTFFloat(TwoFace.Char(us('\u0002')) * TwoFace.Float(us(1.0f)), us(2.0f))
  property("Safe Char * Safe Double") = verifyTFDouble(TwoFace.Char('\u0002') * TwoFace.Double(1.0), 2.0)
  property("Safe Char * Unsafe Double") = verifyTFDouble(TwoFace.Char('\u0002') * TwoFace.Double(us(1.0)), us(2.0))
  property("Unsafe Char * Safe Double") = verifyTFDouble(TwoFace.Char(us('\u0002')) * TwoFace.Double(1.0), us(2.0))
  property("Unsafe Char * Unsafe Double") = verifyTFDouble(TwoFace.Char(us('\u0002')) * TwoFace.Double(us(1.0)), us(2.0))

  property("Safe Char / Safe Char") = verifyTFInt(TwoFace.Char('\u0006') / TwoFace.Char('\u0002'), 3)
  property("Safe Char / Unsafe Char") = verifyTFInt(TwoFace.Char('\u0006') / TwoFace.Char(us('\u0002')), us(3))
  property("Unsafe Char / Safe Char") = verifyTFInt(TwoFace.Char(us('\u0006')) / TwoFace.Char('\u0002'), us(3))
  property("Unsafe Char / Unsafe Char") = verifyTFInt(TwoFace.Char(us('\u0006')) / TwoFace.Char(us('\u0002')), us(3))
  property("Safe Char / Safe Int") = verifyTFInt(TwoFace.Char('\u0006') / TwoFace.Int(2), 3)
  property("Safe Char / Unsafe Int") = verifyTFInt(TwoFace.Char('\u0006') / TwoFace.Int(us(2)), us(3))
  property("Unsafe Char / Safe Int") = verifyTFInt(TwoFace.Char(us('\u0006')) / TwoFace.Int(2), us(3))
  property("Unsafe Char / Unsafe Int") = verifyTFInt(TwoFace.Char(us('\u0006')) / TwoFace.Int(us(2)), us(3))
  property("Safe Char / Safe Long") = verifyTFLong(TwoFace.Char('\u0006') / TwoFace.Long(2L), 3L)
  property("Safe Char / Unsafe Long") = verifyTFLong(TwoFace.Char('\u0006') / TwoFace.Long(us(2L)), us(3L))
  property("Unsafe Char / Safe Long") = verifyTFLong(TwoFace.Char(us('\u0006')) / TwoFace.Long(2L), us(3L))
  property("Unsafe Char / Unsafe Long") = verifyTFLong(TwoFace.Char(us('\u0006')) / TwoFace.Long(us(2L)), us(3L))
  property("Safe Char / Safe Float") = verifyTFFloat(TwoFace.Char('\u0006') / TwoFace.Float(2.0f), 3.0f)
  property("Safe Char / Unsafe Float") = verifyTFFloat(TwoFace.Char('\u0006') / TwoFace.Float(us(2.0f)), us(3.0f))
  property("Unsafe Char / Safe Float") = verifyTFFloat(TwoFace.Char(us('\u0006')) / TwoFace.Float(2.0f), us(3.0f))
  property("Unsafe Char / Unsafe Float") = verifyTFFloat(TwoFace.Char(us('\u0006')) / TwoFace.Float(us(2.0f)), us(3.0f))
  property("Safe Char / Safe Double") = verifyTFDouble(TwoFace.Char('\u0006') / TwoFace.Double(2.0), 3.0)
  property("Safe Char / Unsafe Double") = verifyTFDouble(TwoFace.Char('\u0006') / TwoFace.Double(us(2.0)), us(3.0))
  property("Unsafe Char / Safe Double") = verifyTFDouble(TwoFace.Char(us('\u0006')) / TwoFace.Double(2.0), us(3.0))
  property("Unsafe Char / Unsafe Double") = verifyTFDouble(TwoFace.Char(us('\u0006')) / TwoFace.Double(us(2.0)), us(3.0))

  property("Safe Char % Safe Char") = verifyTFInt(TwoFace.Char('\u0007') % TwoFace.Char('\u0004'), 3)
  property("Safe Char % Unsafe Char") = verifyTFInt(TwoFace.Char('\u0007') % TwoFace.Char(us('\u0004')), us(3))
  property("Unsafe Char % Safe Char") = verifyTFInt(TwoFace.Char(us('\u0007')) % TwoFace.Char('\u0004'), us(3))
  property("Unsafe Char % Unsafe Char") = verifyTFInt(TwoFace.Char(us('\u0007')) % TwoFace.Char(us('\u0004')), us(3))
  property("Safe Char % Safe Int") = verifyTFInt(TwoFace.Char('\u0007') % TwoFace.Int(4), 3)
  property("Safe Char % Unsafe Int") = verifyTFInt(TwoFace.Char('\u0007') % TwoFace.Int(us(4)), us(3))
  property("Unsafe Char % Safe Int") = verifyTFInt(TwoFace.Char(us('\u0007')) % TwoFace.Int(4), us(3))
  property("Unsafe Char % Unsafe Int") = verifyTFInt(TwoFace.Char(us('\u0007')) % TwoFace.Int(us(4)), us(3))
  property("Safe Char % Safe Long") = verifyTFLong(TwoFace.Char('\u0007') % TwoFace.Long(4L), 3L)
  property("Safe Char % Unsafe Long") = verifyTFLong(TwoFace.Char('\u0007') % TwoFace.Long(us(4L)), us(3L))
  property("Unsafe Char % Safe Long") = verifyTFLong(TwoFace.Char(us('\u0007')) % TwoFace.Long(4L), us(3L))
  property("Unsafe Char % Unsafe Long") = verifyTFLong(TwoFace.Char(us('\u0007')) % TwoFace.Long(us(4L)), us(3L))
  property("Safe Char % Safe Float") = verifyTFFloat(TwoFace.Char('\u0007') % TwoFace.Float(4.0f), 3.0f)
  property("Safe Char % Unsafe Float") = verifyTFFloat(TwoFace.Char('\u0007') % TwoFace.Float(us(4.0f)), us(3.0f))
  property("Unsafe Char % Safe Float") = verifyTFFloat(TwoFace.Char(us('\u0007')) % TwoFace.Float(4.0f), us(3.0f))
  property("Unsafe Char % Unsafe Float") = verifyTFFloat(TwoFace.Char(us('\u0007')) % TwoFace.Float(us(4.0f)), us(3.0f))
  property("Safe Char % Safe Double") = verifyTFDouble(TwoFace.Char('\u0007') % TwoFace.Double(4.0), 3.0)
  property("Safe Char % Unsafe Double") = verifyTFDouble(TwoFace.Char('\u0007') % TwoFace.Double(us(4.0)), us(3.0))
  property("Unsafe Char % Safe Double") = verifyTFDouble(TwoFace.Char(us('\u0007')) % TwoFace.Double(4.0), us(3.0))
  property("Unsafe Char % Unsafe Double") = verifyTFDouble(TwoFace.Char(us('\u0007')) % TwoFace.Double(us(4.0)), us(3.0))

  property("Safe Char < Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Char('\u0004'), false)
  property("Safe Char < Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Char < Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Char('\u0004'), us(false))
  property("Unsafe Char < Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Char(us('\u0004')), us(false))
  property("Safe Char < Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Int(4), false)
  property("Safe Char < Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Int(us(4)), us(false))
  property("Unsafe Char < Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Int(4), us(false))
  property("Unsafe Char < Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Int(us(4)), us(false))
  property("Safe Char < Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Long(4L), false)
  property("Safe Char < Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Long(us(4L)), us(false))
  property("Unsafe Char < Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Long(4L), us(false))
  property("Unsafe Char < Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Long(us(4L)), us(false))
  property("Safe Char < Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Float(4.0f), false)
  property("Safe Char < Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Char < Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Float(4.0f), us(false))
  property("Unsafe Char < Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Float(us(4.0f)), us(false))
  property("Safe Char < Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Double(4.0), false)
  property("Safe Char < Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') < TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Char < Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Double(4.0), us(false))
  property("Unsafe Char < Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) < TwoFace.Double(us(4.0)), us(false))

  property("Safe Char > Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Char('\u0004'), true)
  property("Safe Char > Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Char > Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Char('\u0004'), us(true))
  property("Unsafe Char > Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Char(us('\u0004')), us(true))
  property("Safe Char > Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Int(4), true)
  property("Safe Char > Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Int(us(4)), us(true))
  property("Unsafe Char > Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Int(4), us(true))
  property("Unsafe Char > Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Int(us(4)), us(true))
  property("Safe Char > Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Long(4L), true)
  property("Safe Char > Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Long(us(4L)), us(true))
  property("Unsafe Char > Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Long(4L), us(true))
  property("Unsafe Char > Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Long(us(4L)), us(true))
  property("Safe Char > Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Float(4.0f), true)
  property("Safe Char > Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Char > Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Float(4.0f), us(true))
  property("Unsafe Char > Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Float(us(4.0f)), us(true))
  property("Safe Char > Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Double(4.0), true)
  property("Safe Char > Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') > TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Char > Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Double(4.0), us(true))
  property("Unsafe Char > Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) > TwoFace.Double(us(4.0)), us(true))

  property("Safe Char <= Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Char('\u0004'), false)
  property("Safe Char <= Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Char(us('\u0004')), us(false))
  property("Unsafe Char <= Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Char('\u0004'), us(false))
  property("Unsafe Char <= Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Char(us('\u0004')), us(false))
  property("Safe Char <= Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Int(4), false)
  property("Safe Char <= Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Int(us(4)), us(false))
  property("Unsafe Char <= Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Int(4), us(false))
  property("Unsafe Char <= Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Int(us(4)), us(false))
  property("Safe Char <= Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Long(4L), false)
  property("Safe Char <= Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Long(us(4L)), us(false))
  property("Unsafe Char <= Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Long(4L), us(false))
  property("Unsafe Char <= Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Long(us(4L)), us(false))
  property("Safe Char <= Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Float(4.0f), false)
  property("Safe Char <= Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Float(us(4.0f)), us(false))
  property("Unsafe Char <= Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Float(4.0f), us(false))
  property("Unsafe Char <= Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Float(us(4.0f)), us(false))
  property("Safe Char <= Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Double(4.0), false)
  property("Safe Char <= Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') <= TwoFace.Double(us(4.0)), us(false))
  property("Unsafe Char <= Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Double(4.0), us(false))
  property("Unsafe Char <= Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) <= TwoFace.Double(us(4.0)), us(false))

  property("Safe Char >= Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Char('\u0004'), true)
  property("Safe Char >= Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Char(us('\u0004')), us(true))
  property("Unsafe Char >= Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Char('\u0004'), us(true))
  property("Unsafe Char >= Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Char(us('\u0004')), us(true))
  property("Safe Char >= Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Int(4), true)
  property("Safe Char >= Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Int(us(4)), us(true))
  property("Unsafe Char >= Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Int(4), us(true))
  property("Unsafe Char >= Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Int(us(4)), us(true))
  property("Safe Char >= Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Long(4L), true)
  property("Safe Char >= Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Long(us(4L)), us(true))
  property("Unsafe Char >= Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Long(4L), us(true))
  property("Unsafe Char >= Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Long(us(4L)), us(true))
  property("Safe Char >= Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Float(4.0f), true)
  property("Safe Char >= Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Float(us(4.0f)), us(true))
  property("Unsafe Char >= Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Float(4.0f), us(true))
  property("Unsafe Char >= Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Float(us(4.0f)), us(true))
  property("Safe Char >= Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Double(4.0), true)
  property("Safe Char >= Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') >= TwoFace.Double(us(4.0)), us(true))
  property("Unsafe Char >= Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Double(4.0), us(true))
  property("Unsafe Char >= Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) >= TwoFace.Double(us(4.0)), us(true))

  property("Safe Char == Regular Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') == ('\u0007'), true)
  property("Safe Char == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') == (us('\u0007')), us(true))
  property("Unsafe Char == Regular Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == ('\u0007'), us(true))
  property("Unsafe Char == Regular Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (us('\u0007')), us(true))
  property("Safe Char == Regular Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') == (7), true)
  property("Safe Char == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') == (us(7)), us(true))
  property("Unsafe Char == Regular Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (7), us(true))
  property("Unsafe Char == Regular Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (us(7)), us(true))
  property("Safe Char == Regular Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') == (7L), true)
  property("Safe Char == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') == (us(7L)), us(true))
  property("Unsafe Char == Regular Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (7L), us(true))
  property("Unsafe Char == Regular Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (us(7L)), us(true))
  property("Safe Char == Regular Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') == (7.0f), true)
  property("Safe Char == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') == (us(7.0f)), us(true))
  property("Unsafe Char == Regular Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (7.0f), us(true))
  property("Unsafe Char == Regular Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (us(7.0f)), us(true))
  property("Safe Char == Regular Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') == (7.0), true)
  property("Safe Char == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') == (us(7.0)), us(true))
  property("Unsafe Char == Regular Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (7.0), us(true))
  property("Unsafe Char == Regular Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == (us(7.0)), us(true))

  property("Safe Char != Regular Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') != ('\u0007'), false)
  property("Safe Char != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') != (us('\u0007')), us(false))
  property("Unsafe Char != Regular Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != ('\u0007'), us(false))
  property("Unsafe Char != Regular Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (us('\u0007')), us(false))
  property("Safe Char != Regular Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') != (7), false)
  property("Safe Char != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') != (us(7)), us(false))
  property("Unsafe Char != Regular Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (7), us(false))
  property("Unsafe Char != Regular Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (us(7)), us(false))
  property("Safe Char != Regular Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') != (7L), false)
  property("Safe Char != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') != (us(7L)), us(false))
  property("Unsafe Char != Regular Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (7L), us(false))
  property("Unsafe Char != Regular Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (us(7L)), us(false))
  property("Safe Char != Regular Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') != (7.0f), false)
  property("Safe Char != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') != (us(7.0f)), us(false))
  property("Unsafe Char != Regular Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (7.0f), us(false))
  property("Unsafe Char != Regular Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (us(7.0f)), us(false))
  property("Safe Char != Regular Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') != (7.0), false)
  property("Safe Char != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') != (us(7.0)), us(false))
  property("Unsafe Char != Regular Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (7.0), us(false))
  property("Unsafe Char != Regular Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != (us(7.0)), us(false))

  property("Safe Char == Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Char('\u0007'), true)
  property("Safe Char == Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Char(us('\u0007')), us(true))
  property("Unsafe Char == Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Char('\u0007'), us(true))
  property("Unsafe Char == Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Char(us('\u0007')), us(true))
  property("Safe Char == Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Int(7), true)
  property("Safe Char == Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Int(us(7)), us(true))
  property("Unsafe Char == Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Int(7), us(true))
  property("Unsafe Char == Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Int(us(7)), us(true))
  property("Safe Char == Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Long(7L), true)
  property("Safe Char == Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Long(us(7L)), us(true))
  property("Unsafe Char == Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Long(7L), us(true))
  property("Unsafe Char == Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Long(us(7L)), us(true))
  property("Safe Char == Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Float(7.0f), true)
  property("Safe Char == Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Float(us(7.0f)), us(true))
  property("Unsafe Char == Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Float(7.0f), us(true))
  property("Unsafe Char == Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Float(us(7.0f)), us(true))
  property("Safe Char == Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Double(7.0), true)
  property("Safe Char == Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') == TwoFace.Double(us(7.0)), us(true))
  property("Unsafe Char == Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Double(7.0), us(true))
  property("Unsafe Char == Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) == TwoFace.Double(us(7.0)), us(true))

  property("Safe Char != Safe Char") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Char('\u0007'), false)
  property("Safe Char != Unsafe Char") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Char(us('\u0007')), us(false))
  property("Unsafe Char != Safe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Char('\u0007'), us(false))
  property("Unsafe Char != Unsafe Char") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Char(us('\u0007')), us(false))
  property("Safe Char != Safe Int") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Int(7), false)
  property("Safe Char != Unsafe Int") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Int(us(7)), us(false))
  property("Unsafe Char != Safe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Int(7), us(false))
  property("Unsafe Char != Unsafe Int") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Int(us(7)), us(false))
  property("Safe Char != Safe Long") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Long(7L), false)
  property("Safe Char != Unsafe Long") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Long(us(7L)), us(false))
  property("Unsafe Char != Safe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Long(7L), us(false))
  property("Unsafe Char != Unsafe Long") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Long(us(7L)), us(false))
  property("Safe Char != Safe Float") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Float(7.0f), false)
  property("Safe Char != Unsafe Float") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Float(us(7.0f)), us(false))
  property("Unsafe Char != Safe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Float(7.0f), us(false))
  property("Unsafe Char != Unsafe Float") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Float(us(7.0f)), us(false))
  property("Safe Char != Safe Double") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Double(7.0), false)
  property("Safe Char != Unsafe Double") = verifyTFBoolean(TwoFace.Char('\u0007') != TwoFace.Double(us(7.0)), us(false))
  property("Unsafe Char != Safe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Double(7.0), us(false))
  property("Unsafe Char != Unsafe Double") = verifyTFBoolean(TwoFace.Char(us('\u0007')) != TwoFace.Double(us(7.0)), us(false))

  property("Safe Negate") = verifyTFInt(-TwoFace.Char('\u0002'), -2)
  property("Unsafe Negate") = verifyTFInt(-TwoFace.Char(us('\u0002')), us(-2))

  property("Safe toNat") = wellTyped {
    val nat = TwoFace.Char('\u0002').toNat
    verifyOp[nat.N, shapeless.Nat._2]
  }
  property("Safe toInt") = verifyTFInt(TwoFace.Char('\u0001').toInt, 1)
  property("Unsafe toInt") = verifyTFInt(TwoFace.Char(us('\u0001')).toInt, us(1))
  property("Safe toLong") = verifyTFLong(TwoFace.Char('\u0001').toLong, 1L)
  property("Unsafe toLong") = verifyTFLong(TwoFace.Char(us('\u0001')).toLong, us(1L))
  property("Safe toFloat") = verifyTFFloat(TwoFace.Char('\u0001').toFloat, 1.0f)
  property("Unsafe toFloat") = verifyTFFloat(TwoFace.Char(us('\u0001')).toFloat, us(1.0f))
  property("Safe toDouble") = verifyTFDouble(TwoFace.Char('\u0001').toDouble, 1.0)
  property("Unsafe toDouble") = verifyTFDouble(TwoFace.Char(us('\u0001')).toDouble, us(1.0))
  property("Safe toStringTF") = verifyTFString(TwoFace.Char('t').toStringTF, "t")
  property("Unsafe toStringTF") = verifyTFString(TwoFace.Char(us('t')).toStringTF, us("t"))
  property("Safe toSymbol") = {
    val sym = TwoFace.Char('t').toSymbol
    sym == scala.Symbol("t")
  }

  property("Implicit Conversions") = wellTyped {
    val d : W.`'\u0003'`.T = TwoFace.Char('\u0003')
    val e : Char = TwoFace.Char(us('\u0003'))
    val f : TwoFace.Char[Char] = '\u0003'
  }

  property("ToString") = {
    TwoFace.Char[W.`'t'`.T].toString() == "t"
  }

  type Fin = W.`'\u0003'`.T
  final val fin = '\u0003'

  property("Extracting from an Upper Bounded Numeric") = wellTyped {
    def foo[W](width: TwoFace.Char[W]) = width
    def foo2[R <: Char](r: R) = foo(r)
    val a = foo2(W(fin).value)
    implicitly[a.Out =:= Fin]
    val b = foo2(us(fin))
    implicitly[b.Out =:= Char]
  }

  property("Extracting from Safe TwoFace") = {
    val a = me(TwoFace.Char(fin))
    val ret = shapeless.the[Id[a.T]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  def noImplFoo[W](w : TwoFace.Char[W]) = w.toInt //Missing twoface shell implicit
  property("Unavailable Implicit Safe TwoFace Shell") = {
    val ret = noImplFoo('\u0002')
    implicitly[ret.Out <:< ToInt[W.`'\u0002'`.T]]
    val retSimple = ret.simplify.toChar.simplify
    implicitly[retSimple.Out <:< W.`'\u0002'`.T]
    retSimple.getValue == 2
  }
  property("Unavailable Implicit Unsafe TwoFace Shell") = {
    val ret = noImplFoo(us('\u0002'))
    implicitly[ret.Out <:< ToInt[Char]]
    val retSimple = ret.simplify
    implicitly[retSimple.Out <:< Int]
    retSimple.getValue == 2
  }
}