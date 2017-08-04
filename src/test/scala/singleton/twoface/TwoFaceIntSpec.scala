package singleton.twoface

import singleton.twoface.math._
import org.scalacheck.Properties
import singleton.TestUtils._
import shapeless.test.illTyped
import singleton.ops._

class TwoFaceIntSpec extends Properties("TwoFace.Int") {
  property("Implicit Creation[]") = {
    val a = implicitly[TwoFace.Int[W.`2`.T]]
    a.getValue == 2 && a.isLiteral
  }
  property("Safe Creation[]") = {
    val a = TwoFace.Int[W.`2`.T]
    a.getValue == 2 && a.isLiteral
  }
  property("Safe Creation()") = {
    val a = TwoFace.Int(2)
    a.getValue == 2 && a.isLiteral
  }
  property("Unsafe Creation()") = {
    val a = TwoFace.Int(us(2))
    a.getValue == 2 && !a.isLiteral
  }

//  property("Safe ifThenElse") = verifyTF(ifThenElse(true, 1, 2), 1)
//  property("Unsafe ifThenElse") = verifyTF(ifThenElse(us(false), 1, 2), us(2))
//
//  property("Safe Int + Safe Char") = verifyTF(TwoFace.Int(2) + TwoFace.Char('\u0001'), 3)
//  property("Safe Int + Unsafe Char") = verifyTF(TwoFace.Int(2) + TwoFace.Char(us('\u0001')), us(3))
//  property("Unsafe Int + Safe Char") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Char('\u0001'), us(3))
//  property("Unsafe Int + Unsafe Char") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Char(us('\u0001')), us(3))
  property("Safe Int + Safe Int") = verifyTF(TwoFace.Int(2) + TwoFace.Int(1), 3)
  property("Safe Int + Unsafe Int") = verifyTF(TwoFace.Int(2) + TwoFace.Int(us(1)), us(3))
  property("Unsafe Int + Safe Int") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Int(1), us(3))
  property("Unsafe Int + Unsafe Int") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Int(us(1)), us(3))
//  property("Safe Int + Safe Long") = verifyTF(TwoFace.Int(2) + TwoFace.Long(1L), 3L)
//  property("Safe Int + Unsafe Long") = verifyTF(TwoFace.Int(2) + TwoFace.Long(us(1L)), us(3L))
//  property("Unsafe Int + Safe Long") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Long(1L), us(3L))
//  property("Unsafe Int + Unsafe Long") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Long(us(1L)), us(3L))
//  property("Safe Int + Safe Float") = verifyTF(TwoFace.Int(2) + TwoFace.Float(1.0f), 3.0f)
//  property("Safe Int + Unsafe Float") = verifyTF(TwoFace.Int(2) + TwoFace.Float(us(1.0f)), us(3.0f))
//  property("Unsafe Int + Safe Float") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Float(1.0f), us(3.0f))
//  property("Unsafe Int + Unsafe Float") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Float(us(1.0f)), us(3.0f))
//  property("Safe Int + Safe Double") = verifyTF(TwoFace.Int(2) + TwoFace.Double(1.0), 3.0)
//  property("Safe Int + Unsafe Double") = verifyTF(TwoFace.Int(2) + TwoFace.Double(us(1.0)), us(3.0))
//  property("Unsafe Int + Safe Double") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Double(1.0), us(3.0))
//  property("Unsafe Int + Unsafe Double") = verifyTF(TwoFace.Int(us(2)) + TwoFace.Double(us(1.0)), us(3.0))
//
//  property("Safe Int - Safe Char") = verifyTF(TwoFace.Int(2) - TwoFace.Char('\u0001'), 1)
//  property("Safe Int - Unsafe Char") = verifyTF(TwoFace.Int(2) - TwoFace.Char(us('\u0001')), us(1))
//  property("Unsafe Int - Safe Char") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Char('\u0001'), us(1))
//  property("Unsafe Int - Unsafe Char") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Char(us('\u0001')), us(1))
//  property("Safe Int - Safe Int") = verifyTF(TwoFace.Int(2) - TwoFace.Int(1), 1)
//  property("Safe Int - Unsafe Int") = verifyTF(TwoFace.Int(2) - TwoFace.Int(us(1)), us(1))
//  property("Unsafe Int - Safe Int") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Int(1), us(1))
//  property("Unsafe Int - Unsafe Int") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Int(us(1)), us(1))
//  property("Safe Int - Safe Long") = verifyTF(TwoFace.Int(2) - TwoFace.Long(1L), 1L)
//  property("Safe Int - Unsafe Long") = verifyTF(TwoFace.Int(2) - TwoFace.Long(us(1L)), us(1L))
//  property("Unsafe Int - Safe Long") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Long(1L), us(1L))
//  property("Unsafe Int - Unsafe Long") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Long(us(1L)), us(1L))
//  property("Safe Int - Safe Float") = verifyTF(TwoFace.Int(2) - TwoFace.Float(1.0f), 1.0f)
//  property("Safe Int - Unsafe Float") = verifyTF(TwoFace.Int(2) - TwoFace.Float(us(1.0f)), us(1.0f))
//  property("Unsafe Int - Safe Float") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Float(1.0f), us(1.0f))
//  property("Unsafe Int - Unsafe Float") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Float(us(1.0f)), us(1.0f))
//  property("Safe Int - Safe Double") = verifyTF(TwoFace.Int(2) - TwoFace.Double(1.0), 1.0)
//  property("Safe Int - Unsafe Double") = verifyTF(TwoFace.Int(2) - TwoFace.Double(us(1.0)), us(1.0))
//  property("Unsafe Int - Safe Double") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Double(1.0), us(1.0))
//  property("Unsafe Int - Unsafe Double") = verifyTF(TwoFace.Int(us(2)) - TwoFace.Double(us(1.0)), us(1.0))
//
//  property("Safe Int * Safe Char") = verifyTF(TwoFace.Int(2) * TwoFace.Char('\u0001'), 2)
//  property("Safe Int * Unsafe Char") = verifyTF(TwoFace.Int(2) * TwoFace.Char(us('\u0001')), us(2))
//  property("Unsafe Int * Safe Char") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Char('\u0001'), us(2))
//  property("Unsafe Int * Unsafe Char") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Char(us('\u0001')), us(2))
//  property("Safe Int * Safe Int") = verifyTF(TwoFace.Int(2) * TwoFace.Int(1), 2)
//  property("Safe Int * Unsafe Int") = verifyTF(TwoFace.Int(2) * TwoFace.Int(us(1)), us(2))
//  property("Unsafe Int * Safe Int") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Int(1), us(2))
//  property("Unsafe Int * Unsafe Int") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Int(us(1)), us(2))
//  property("Safe Int * Safe Long") = verifyTF(TwoFace.Int(2) * TwoFace.Long(1L), 2L)
//  property("Safe Int * Unsafe Long") = verifyTF(TwoFace.Int(2) * TwoFace.Long(us(1L)), us(2L))
//  property("Unsafe Int * Safe Long") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Long(1L), us(2L))
//  property("Unsafe Int * Unsafe Long") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Long(us(1L)), us(2L))
//  property("Safe Int * Safe Float") = verifyTF(TwoFace.Int(2) * TwoFace.Float(1.0f), 2.0f)
//  property("Safe Int * Unsafe Float") = verifyTF(TwoFace.Int(2) * TwoFace.Float(us(1.0f)), us(2.0f))
//  property("Unsafe Int * Safe Float") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Float(1.0f), us(2.0f))
//  property("Unsafe Int * Unsafe Float") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Float(us(1.0f)), us(2.0f))
//  property("Safe Int * Safe Double") = verifyTF(TwoFace.Int(2) * TwoFace.Double(1.0), 2.0)
//  property("Safe Int * Unsafe Double") = verifyTF(TwoFace.Int(2) * TwoFace.Double(us(1.0)), us(2.0))
//  property("Unsafe Int * Safe Double") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Double(1.0), us(2.0))
//  property("Unsafe Int * Unsafe Double") = verifyTF(TwoFace.Int(us(2)) * TwoFace.Double(us(1.0)), us(2.0))
//
//  property("Safe Int / Safe Char") = verifyTF(TwoFace.Int(6) / TwoFace.Char('\u0002'), 3)
//  property("Safe Int / Unsafe Char") = verifyTF(TwoFace.Int(6) / TwoFace.Char(us('\u0002')), us(3))
//  property("Unsafe Int / Safe Char") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Char('\u0002'), us(3))
//  property("Unsafe Int / Unsafe Char") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Char(us('\u0002')), us(3))
//  property("Safe Int / Safe Int") = verifyTF(TwoFace.Int(6) / TwoFace.Int(2), 3)
//  property("Safe Int / Unsafe Int") = verifyTF(TwoFace.Int(6) / TwoFace.Int(us(2)), us(3))
//  property("Unsafe Int / Safe Int") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Int(2), us(3))
//  property("Unsafe Int / Unsafe Int") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Int(us(2)), us(3))
//  property("Safe Int / Safe Long") = verifyTF(TwoFace.Int(6) / TwoFace.Long(2L), 3L)
//  property("Safe Int / Unsafe Long") = verifyTF(TwoFace.Int(6) / TwoFace.Long(us(2L)), us(3L))
//  property("Unsafe Int / Safe Long") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Long(2L), us(3L))
//  property("Unsafe Int / Unsafe Long") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Long(us(2L)), us(3L))
//  property("Safe Int / Safe Float") = verifyTF(TwoFace.Int(6) / TwoFace.Float(2.0f), 3.0f)
//  property("Safe Int / Unsafe Float") = verifyTF(TwoFace.Int(6) / TwoFace.Float(us(2.0f)), us(3.0f))
//  property("Unsafe Int / Safe Float") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Float(2.0f), us(3.0f))
//  property("Unsafe Int / Unsafe Float") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Float(us(2.0f)), us(3.0f))
//  property("Safe Int / Safe Double") = verifyTF(TwoFace.Int(6) / TwoFace.Double(2.0), 3.0)
//  property("Safe Int / Unsafe Double") = verifyTF(TwoFace.Int(6) / TwoFace.Double(us(2.0)), us(3.0))
//  property("Unsafe Int / Safe Double") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Double(2.0), us(3.0))
//  property("Unsafe Int / Unsafe Double") = verifyTF(TwoFace.Int(us(6)) / TwoFace.Double(us(2.0)), us(3.0))
//
//  property("Safe Int % Safe Char") = verifyTF(TwoFace.Int(7) % TwoFace.Char('\u0004'), 3)
//  property("Safe Int % Unsafe Char") = verifyTF(TwoFace.Int(7) % TwoFace.Char(us('\u0004')), us(3))
//  property("Unsafe Int % Safe Char") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Char('\u0004'), us(3))
//  property("Unsafe Int % Unsafe Char") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Char(us('\u0004')), us(3))
//  property("Safe Int % Safe Int") = verifyTF(TwoFace.Int(7) % TwoFace.Int(4), 3)
//  property("Safe Int % Unsafe Int") = verifyTF(TwoFace.Int(7) % TwoFace.Int(us(4)), us(3))
//  property("Unsafe Int % Safe Int") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Int(4), us(3))
//  property("Unsafe Int % Unsafe Int") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Int(us(4)), us(3))
//  property("Safe Int % Safe Long") = verifyTF(TwoFace.Int(7) % TwoFace.Long(4L), 3L)
//  property("Safe Int % Unsafe Long") = verifyTF(TwoFace.Int(7) % TwoFace.Long(us(4L)), us(3L))
//  property("Unsafe Int % Safe Long") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Long(4L), us(3L))
//  property("Unsafe Int % Unsafe Long") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Long(us(4L)), us(3L))
//  property("Safe Int % Safe Float") = verifyTF(TwoFace.Int(7) % TwoFace.Float(4.0f), 3.0f)
//  property("Safe Int % Unsafe Float") = verifyTF(TwoFace.Int(7) % TwoFace.Float(us(4.0f)), us(3.0f))
//  property("Unsafe Int % Safe Float") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Float(4.0f), us(3.0f))
//  property("Unsafe Int % Unsafe Float") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Float(us(4.0f)), us(3.0f))
//  property("Safe Int % Safe Double") = verifyTF(TwoFace.Int(7) % TwoFace.Double(4.0), 3.0)
//  property("Safe Int % Unsafe Double") = verifyTF(TwoFace.Int(7) % TwoFace.Double(us(4.0)), us(3.0))
//  property("Unsafe Int % Safe Double") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Double(4.0), us(3.0))
//  property("Unsafe Int % Unsafe Double") = verifyTF(TwoFace.Int(us(7)) % TwoFace.Double(us(4.0)), us(3.0))
//
//  property("Safe Int < Safe Char") = verifyTF(TwoFace.Int(7) < TwoFace.Char('\u0004'), false)
//  property("Safe Int < Unsafe Char") = verifyTF(TwoFace.Int(7) < TwoFace.Char(us('\u0004')), us(false))
//  property("Unsafe Int < Safe Char") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Char('\u0004'), us(false))
//  property("Unsafe Int < Unsafe Char") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Char(us('\u0004')), us(false))
//  property("Safe Int < Safe Int") = verifyTF(TwoFace.Int(7) < TwoFace.Int(4), false)
//  property("Safe Int < Unsafe Int") = verifyTF(TwoFace.Int(7) < TwoFace.Int(us(4)), us(false))
//  property("Unsafe Int < Safe Int") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Int(4), us(false))
//  property("Unsafe Int < Unsafe Int") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Int(us(4)), us(false))
//  property("Safe Int < Safe Long") = verifyTF(TwoFace.Int(7) < TwoFace.Long(4L), false)
//  property("Safe Int < Unsafe Long") = verifyTF(TwoFace.Int(7) < TwoFace.Long(us(4L)), us(false))
//  property("Unsafe Int < Safe Long") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Long(4L), us(false))
//  property("Unsafe Int < Unsafe Long") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Long(us(4L)), us(false))
//  property("Safe Int < Safe Float") = verifyTF(TwoFace.Int(7) < TwoFace.Float(4.0f), false)
//  property("Safe Int < Unsafe Float") = verifyTF(TwoFace.Int(7) < TwoFace.Float(us(4.0f)), us(false))
//  property("Unsafe Int < Safe Float") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Float(4.0f), us(false))
//  property("Unsafe Int < Unsafe Float") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Float(us(4.0f)), us(false))
//  property("Safe Int < Safe Double") = verifyTF(TwoFace.Int(7) < TwoFace.Double(4.0), false)
//  property("Safe Int < Unsafe Double") = verifyTF(TwoFace.Int(7) < TwoFace.Double(us(4.0)), us(false))
//  property("Unsafe Int < Safe Double") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Double(4.0), us(false))
//  property("Unsafe Int < Unsafe Double") = verifyTF(TwoFace.Int(us(7)) < TwoFace.Double(us(4.0)), us(false))
//
//  property("Safe Int > Safe Char") = verifyTF(TwoFace.Int(7) > TwoFace.Char('\u0004'), true)
//  property("Safe Int > Unsafe Char") = verifyTF(TwoFace.Int(7) > TwoFace.Char(us('\u0004')), us(true))
//  property("Unsafe Int > Safe Char") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Char('\u0004'), us(true))
//  property("Unsafe Int > Unsafe Char") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Char(us('\u0004')), us(true))
//  property("Safe Int > Safe Int") = verifyTF(TwoFace.Int(7) > TwoFace.Int(4), true)
//  property("Safe Int > Unsafe Int") = verifyTF(TwoFace.Int(7) > TwoFace.Int(us(4)), us(true))
//  property("Unsafe Int > Safe Int") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Int(4), us(true))
//  property("Unsafe Int > Unsafe Int") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Int(us(4)), us(true))
//  property("Safe Int > Safe Long") = verifyTF(TwoFace.Int(7) > TwoFace.Long(4L), true)
//  property("Safe Int > Unsafe Long") = verifyTF(TwoFace.Int(7) > TwoFace.Long(us(4L)), us(true))
//  property("Unsafe Int > Safe Long") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Long(4L), us(true))
//  property("Unsafe Int > Unsafe Long") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Long(us(4L)), us(true))
//  property("Safe Int > Safe Float") = verifyTF(TwoFace.Int(7) > TwoFace.Float(4.0f), true)
//  property("Safe Int > Unsafe Float") = verifyTF(TwoFace.Int(7) > TwoFace.Float(us(4.0f)), us(true))
//  property("Unsafe Int > Safe Float") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Float(4.0f), us(true))
//  property("Unsafe Int > Unsafe Float") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Float(us(4.0f)), us(true))
//  property("Safe Int > Safe Double") = verifyTF(TwoFace.Int(7) > TwoFace.Double(4.0), true)
//  property("Safe Int > Unsafe Double") = verifyTF(TwoFace.Int(7) > TwoFace.Double(us(4.0)), us(true))
//  property("Unsafe Int > Safe Double") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Double(4.0), us(true))
//  property("Unsafe Int > Unsafe Double") = verifyTF(TwoFace.Int(us(7)) > TwoFace.Double(us(4.0)), us(true))
//
//  property("Safe Int <= Safe Char") = verifyTF(TwoFace.Int(7) <= TwoFace.Char('\u0004'), false)
//  property("Safe Int <= Unsafe Char") = verifyTF(TwoFace.Int(7) <= TwoFace.Char(us('\u0004')), us(false))
//  property("Unsafe Int <= Safe Char") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Char('\u0004'), us(false))
//  property("Unsafe Int <= Unsafe Char") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Char(us('\u0004')), us(false))
//  property("Safe Int <= Safe Int") = verifyTF(TwoFace.Int(7) <= TwoFace.Int(4), false)
//  property("Safe Int <= Unsafe Int") = verifyTF(TwoFace.Int(7) <= TwoFace.Int(us(4)), us(false))
//  property("Unsafe Int <= Safe Int") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Int(4), us(false))
//  property("Unsafe Int <= Unsafe Int") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Int(us(4)), us(false))
//  property("Safe Int <= Safe Long") = verifyTF(TwoFace.Int(7) <= TwoFace.Long(4L), false)
//  property("Safe Int <= Unsafe Long") = verifyTF(TwoFace.Int(7) <= TwoFace.Long(us(4L)), us(false))
//  property("Unsafe Int <= Safe Long") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Long(4L), us(false))
//  property("Unsafe Int <= Unsafe Long") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Long(us(4L)), us(false))
//  property("Safe Int <= Safe Float") = verifyTF(TwoFace.Int(7) <= TwoFace.Float(4.0f), false)
//  property("Safe Int <= Unsafe Float") = verifyTF(TwoFace.Int(7) <= TwoFace.Float(us(4.0f)), us(false))
//  property("Unsafe Int <= Safe Float") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Float(4.0f), us(false))
//  property("Unsafe Int <= Unsafe Float") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Float(us(4.0f)), us(false))
//  property("Safe Int <= Safe Double") = verifyTF(TwoFace.Int(7) <= TwoFace.Double(4.0), false)
//  property("Safe Int <= Unsafe Double") = verifyTF(TwoFace.Int(7) <= TwoFace.Double(us(4.0)), us(false))
//  property("Unsafe Int <= Safe Double") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Double(4.0), us(false))
//  property("Unsafe Int <= Unsafe Double") = verifyTF(TwoFace.Int(us(7)) <= TwoFace.Double(us(4.0)), us(false))
//
//  property("Safe Int >= Safe Char") = verifyTF(TwoFace.Int(7) >= TwoFace.Char('\u0004'), true)
//  property("Safe Int >= Unsafe Char") = verifyTF(TwoFace.Int(7) >= TwoFace.Char(us('\u0004')), us(true))
//  property("Unsafe Int >= Safe Char") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Char('\u0004'), us(true))
//  property("Unsafe Int >= Unsafe Char") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Char(us('\u0004')), us(true))
//  property("Safe Int >= Safe Int") = verifyTF(TwoFace.Int(7) >= TwoFace.Int(4), true)
//  property("Safe Int >= Unsafe Int") = verifyTF(TwoFace.Int(7) >= TwoFace.Int(us(4)), us(true))
//  property("Unsafe Int >= Safe Int") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Int(4), us(true))
//  property("Unsafe Int >= Unsafe Int") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Int(us(4)), us(true))
//  property("Safe Int >= Safe Long") = verifyTF(TwoFace.Int(7) >= TwoFace.Long(4L), true)
//  property("Safe Int >= Unsafe Long") = verifyTF(TwoFace.Int(7) >= TwoFace.Long(us(4L)), us(true))
//  property("Unsafe Int >= Safe Long") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Long(4L), us(true))
//  property("Unsafe Int >= Unsafe Long") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Long(us(4L)), us(true))
//  property("Safe Int >= Safe Float") = verifyTF(TwoFace.Int(7) >= TwoFace.Float(4.0f), true)
//  property("Safe Int >= Unsafe Float") = verifyTF(TwoFace.Int(7) >= TwoFace.Float(us(4.0f)), us(true))
//  property("Unsafe Int >= Safe Float") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Float(4.0f), us(true))
//  property("Unsafe Int >= Unsafe Float") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Float(us(4.0f)), us(true))
//  property("Safe Int >= Safe Double") = verifyTF(TwoFace.Int(7) >= TwoFace.Double(4.0), true)
//  property("Safe Int >= Unsafe Double") = verifyTF(TwoFace.Int(7) >= TwoFace.Double(us(4.0)), us(true))
//  property("Unsafe Int >= Safe Double") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Double(4.0), us(true))
//  property("Unsafe Int >= Unsafe Double") = verifyTF(TwoFace.Int(us(7)) >= TwoFace.Double(us(4.0)), us(true))
//
//  property("Safe Int == Safe Char") = verifyTF(TwoFace.Int(7) == TwoFace.Char('\u0007'), true)
//  property("Safe Int == Unsafe Char") = verifyTF(TwoFace.Int(7) == TwoFace.Char(us('\u0007')), us(true))
//  property("Unsafe Int == Safe Char") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Char('\u0007'), us(true))
//  property("Unsafe Int == Unsafe Char") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Char(us('\u0007')), us(true))
//  property("Safe Int == Safe Int") = verifyTF(TwoFace.Int(7) == TwoFace.Int(7), true)
//  property("Safe Int == Unsafe Int") = verifyTF(TwoFace.Int(7) == TwoFace.Int(us(7)), us(true))
//  property("Unsafe Int == Safe Int") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Int(7), us(true))
//  property("Unsafe Int == Unsafe Int") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Int(us(7)), us(true))
//  property("Safe Int == Safe Long") = verifyTF(TwoFace.Int(7) == TwoFace.Long(7L), true)
//  property("Safe Int == Unsafe Long") = verifyTF(TwoFace.Int(7) == TwoFace.Long(us(7L)), us(true))
//  property("Unsafe Int == Safe Long") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Long(7L), us(true))
//  property("Unsafe Int == Unsafe Long") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Long(us(7L)), us(true))
//  property("Safe Int == Safe Float") = verifyTF(TwoFace.Int(7) == TwoFace.Float(7.0f), true)
//  property("Safe Int == Unsafe Float") = verifyTF(TwoFace.Int(7) == TwoFace.Float(us(7.0f)), us(true))
//  property("Unsafe Int == Safe Float") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Float(7.0f), us(true))
//  property("Unsafe Int == Unsafe Float") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Float(us(7.0f)), us(true))
//  property("Safe Int == Safe Double") = verifyTF(TwoFace.Int(7) == TwoFace.Double(7.0), true)
//  property("Safe Int == Unsafe Double") = verifyTF(TwoFace.Int(7) == TwoFace.Double(us(7.0)), us(true))
//  property("Unsafe Int == Safe Double") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Double(7.0), us(true))
//  property("Unsafe Int == Unsafe Double") = verifyTF(TwoFace.Int(us(7)) == TwoFace.Double(us(7.0)), us(true))
//
//  property("Safe Int == Regular Safe Char") = verifyTF(TwoFace.Int(7) == ('\u0007'), true)
//  property("Safe Int == Regular Unsafe Char") = verifyTF(TwoFace.Int(7) == (us('\u0007')), us(true))
//  property("Unsafe Int == Regular Safe Char") = verifyTF(TwoFace.Int(us(7)) == ('\u0007'), us(true))
//  property("Unsafe Int == Regular Unsafe Char") = verifyTF(TwoFace.Int(us(7)) == (us('\u0007')), us(true))
//  property("Safe Int == Regular Safe Int") = verifyTF(TwoFace.Int(7) == (7), true)
//  property("Safe Int == Regular Unsafe Int") = verifyTF(TwoFace.Int(7) == (us(7)), us(true))
//  property("Unsafe Int == Regular Safe Int") = verifyTF(TwoFace.Int(us(7)) == (7), us(true))
//  property("Unsafe Int == Regular Unsafe Int") = verifyTF(TwoFace.Int(us(7)) == (us(7)), us(true))
//  property("Safe Int == Regular Safe Long") = verifyTF(TwoFace.Int(7) == (7L), true)
//  property("Safe Int == Regular Unsafe Long") = verifyTF(TwoFace.Int(7) == (us(7L)), us(true))
//  property("Unsafe Int == Regular Safe Long") = verifyTF(TwoFace.Int(us(7)) == (7L), us(true))
//  property("Unsafe Int == Regular Unsafe Long") = verifyTF(TwoFace.Int(us(7)) == (us(7L)), us(true))
//  property("Safe Int == Regular Safe Float") = verifyTF(TwoFace.Int(7) == (7.0f), true)
//  property("Safe Int == Regular Unsafe Float") = verifyTF(TwoFace.Int(7) == (us(7.0f)), us(true))
//  property("Unsafe Int == Regular Safe Float") = verifyTF(TwoFace.Int(us(7)) == (7.0f), us(true))
//  property("Unsafe Int == Regular Unsafe Float") = verifyTF(TwoFace.Int(us(7)) == (us(7.0f)), us(true))
//  property("Safe Int == Regular Safe Double") = verifyTF(TwoFace.Int(7) == (7.0), true)
//  property("Safe Int == Regular Unsafe Double") = verifyTF(TwoFace.Int(7) == (us(7.0)), us(true))
//  property("Unsafe Int == Regular Safe Double") = verifyTF(TwoFace.Int(us(7)) == (7.0), us(true))
//  property("Unsafe Int == Regular Unsafe Double") = verifyTF(TwoFace.Int(us(7)) == (us(7.0)), us(true))
//
//  property("Safe Int != Safe Char") = verifyTF(TwoFace.Int(7) != TwoFace.Char('\u0007'), false)
//  property("Safe Int != Unsafe Char") = verifyTF(TwoFace.Int(7) != TwoFace.Char(us('\u0007')), us(false))
//  property("Unsafe Int != Safe Char") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Char('\u0007'), us(false))
//  property("Unsafe Int != Unsafe Char") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Char(us('\u0007')), us(false))
//  property("Safe Int != Safe Int") = verifyTF(TwoFace.Int(7) != TwoFace.Int(7), false)
//  property("Safe Int != Unsafe Int") = verifyTF(TwoFace.Int(7) != TwoFace.Int(us(7)), us(false))
//  property("Unsafe Int != Safe Int") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Int(7), us(false))
//  property("Unsafe Int != Unsafe Int") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Int(us(7)), us(false))
//  property("Safe Int != Safe Long") = verifyTF(TwoFace.Int(7) != TwoFace.Long(7L), false)
//  property("Safe Int != Unsafe Long") = verifyTF(TwoFace.Int(7) != TwoFace.Long(us(7L)), us(false))
//  property("Unsafe Int != Safe Long") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Long(7L), us(false))
//  property("Unsafe Int != Unsafe Long") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Long(us(7L)), us(false))
//  property("Safe Int != Safe Float") = verifyTF(TwoFace.Int(7) != TwoFace.Float(7.0f), false)
//  property("Safe Int != Unsafe Float") = verifyTF(TwoFace.Int(7) != TwoFace.Float(us(7.0f)), us(false))
//  property("Unsafe Int != Safe Float") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Float(7.0f), us(false))
//  property("Unsafe Int != Unsafe Float") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Float(us(7.0f)), us(false))
//  property("Safe Int != Safe Double") = verifyTF(TwoFace.Int(7) != TwoFace.Double(7.0), false)
//  property("Safe Int != Unsafe Double") = verifyTF(TwoFace.Int(7) != TwoFace.Double(us(7.0)), us(false))
//  property("Unsafe Int != Safe Double") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Double(7.0), us(false))
//  property("Unsafe Int != Unsafe Double") = verifyTF(TwoFace.Int(us(7)) != TwoFace.Double(us(7.0)), us(false))
//
//  property("Safe Int min Safe Int") = verifyTF(min(TwoFace.Int(2), TwoFace.Int(1)), 1)
//  property("Safe Int min Unsafe Int") = verifyTF(min(TwoFace.Int(2), TwoFace.Int(us(1))), us(1))
//  property("Unsafe Int min Safe Int") = verifyTF(min(TwoFace.Int(us(2)), TwoFace.Int(1)), us(1))
//  property("Unsafe Int min Unsafe Int") = verifyTF(min(TwoFace.Int(us(2)), TwoFace.Int(us(1))), us(1))
//
//  property("Safe Int max Safe Int") = verifyTF(max(TwoFace.Int(2), TwoFace.Int(1)), 2)
//  property("Safe Int max Unsafe Int") = verifyTF(max(TwoFace.Int(2), TwoFace.Int(us(1))), us(2))
//  property("Unsafe Int max Safe Int") = verifyTF(max(TwoFace.Int(us(2)), TwoFace.Int(1)), us(2))
//  property("Unsafe Int max Unsafe Int") = verifyTF(max(TwoFace.Int(us(2)), TwoFace.Int(us(1))), us(2))

  property("Safe Negate") = verifyTF(-TwoFace.Int(-1), 1)
  property("Unsafe Negate") = verifyTF(-TwoFace.Int(us(1)), us(-1))

//  property("Safe toChar") = verifyTF(TwoFace.Int(1).toChar, '\u0001')
//  property("Unsafe toChar") = verifyTF(TwoFace.Int(us(1)).toChar, us('\u0001'))
//  property("Safe toLong") = verifyTF(TwoFace.Int(1).toLong, 1L)
//  property("Unsafe toLong") = verifyTF(TwoFace.Int(us(1)).toLong, us(1L))
//  property("Safe toFloat") = verifyTF(TwoFace.Int(1).toFloat, 1.0f)
//  property("Unsafe toFloat") = verifyTF(TwoFace.Int(us(1)).toFloat, us(1.0f))
//  property("Safe toDouble") = verifyTF(TwoFace.Int(1).toDouble, 1.0)
//  property("Unsafe toDouble") = verifyTF(TwoFace.Int(us(1)).toDouble, us(1.0))
//  property("Safe toStringTF") = verifyTF(TwoFace.Int(1).toStringTF, "1")
//  property("Unsafe toStringTF") = verifyTF(TwoFace.Int(us(1)).toStringTF, us("1"))

  property("Safe abs") = verifyTF(abs(TwoFace.Int(-1)), 1)
  property("Unsafe abs") = verifyTF(abs(TwoFace.Int(us(-1))), us(1))

  property("Safe numberOfLeadingZeros") = verifyTF(TwoFace.Int.numberOfLeadingZeros(TwoFace.Int(1)), 31)
  property("Unsafe numberOfLeadingZeros") = verifyTF(TwoFace.Int.numberOfLeadingZeros(TwoFace.Int(us(1))), us(31))

  property("Implicit Conversions") = wellTyped {
    val a : TwoFace.Int[W.`3`.T] = implicitly[TwoFace.Int[W.`2`.T + W.`1`.T]]
    val b : TwoFace.Int[W.`3`.T + W.`0`.T] = implicitly[TwoFace.Int[W.`2`.T + W.`1`.T]]
    val c : TwoFace.Int[W.`3`.T + W.`0`.T] = implicitly[TwoFace.Int[W.`3`.T]]
    val d : W.`3`.T = TwoFace.Int(3)
    val e : Int = TwoFace.Int(us(3))
//    val f : W.`3`.T = shapeless.the[TwoFace.Int[Id[W.`3`.T]]]
  }

  property("Wrong Implicit Conversions") = {
    illTyped("""val a : TwoFace.Int[W.`3`.T] = implicitly[TwoFace.Int[W.`2`.T + W.`2`.T]]""")
    illTyped("""val b : TwoFace.Int[W.`3`.T + W.`0`.T] = implicitly[TwoFace.Int[W.`2`.T + W.`2`.T]]""")
    illTyped("""val c : TwoFace.Int[W.`3`.T + W.`0`.T] = implicitly[TwoFace.Int[W.`4`.T]]""")
    true
  }

  property("ToString") = {
    TwoFace.Int[W.`3`.T].toString() == "1"
  }

  type Fin = W.`3`.T
  final val fin = 3
  property("Extracting from Safe TwoFace") = {
    val a = me(TwoFace.Int(fin))
    val ret = shapeless.the[Id[a.T]]
    implicitly[ret.Out =:= Fin]
    ret.value == fin
  }

  def noImplFoo[W](w : TwoFace.Int[W]) = -w //Missing twoface shell implicit
  property("Unavailable Implicit Safe TwoFace Shell") = {
    val ret = noImplFoo(2)
    implicitly[ret.T <:< Negate[W.`2`.T]]
    val retSimple = ret.simplify
    implicitly[retSimple.T <:< W.`-2`.T]
    retSimple.getValue == -2
  }
  property("Unavailable Implicit Unsafe TwoFace Shell") = {
    val ret = noImplFoo(us(2))
    implicitly[ret.T <:< Negate[Int]]
    val retSimple = ret.simplify
    implicitly[retSimple.T <:< Int]
    retSimple.getValue == -2
  }
}