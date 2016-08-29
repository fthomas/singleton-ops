package MatrixExample

object Test {
  import Implicits._
  val m_3_5 = new Matrix[3, 5, MyElm]
  val m_5_3 = m_3_5.transpose
  val test_add = m_3_5 + m_3_5
  val m_3_3 = m_3_5 * m_5_3
  val m_5_5 = m_5_3 * m_3_5

  val m_1_5 = new Matrix[1, 5, MyElm]
  val m_5_1 = m_1_5.transpose
  val e = m_1_5*m_5_1

  //e.IAmElement //Implicit of M[1,1]
  m_1_5.IAmRVector //Implicit of M[1,C]
  val m_6_6 = m_5_5.promoteBy[1]

  //val test_bad_add = m_5_3 + m_3_5
  //val test_bad_mul = m_3_5 * m_3_5
}



