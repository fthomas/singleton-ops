package singleton.twoface

/**
  * Created by soronpo on 07/05/2017.
  */
import singleton.ops._
//import TwoFace._
//@scala.annotation.implicitNotFound("${Msg}")
trait Checked[Face, Value, Cond, Msg] {
  val value : Face

}
object Checked {
//  implicit def ev[F, V, C, M, TFV <: TwoFace.TwoFaceAny[F, V]](implicit tfv : TFV, ct  : CompileTime[C], di : DummyImplicit) : Checked[F, V, C, M] = new Checked[F, V, C, M] {
//    val value = tfv.getValue
//  }
  implicit def fromInt[V : TwoFaceAny.Int, C, M](tfv : TwoFaceAny.Int[V])(implicit ct  : CompileTime[C]) : Checked[Int, V, C, M] = new Checked[Int, V, C, M] {
    val value = tfv.getValue
  }
}

object Bla {
//  val a : Checked[Int, 5, true, "df"] = 5
//  implicitly[Checked[scala.Int, 5, true, "df"]]
}
//trait CheckedObj[Chk[F, V, C, M] <: Checked[F, V, C, M]] {
//  def create[F, V, C, M](tfv : TwoFaceAny[F, V]) : Chk[F, V, C, M]
//  implicit def ev[F, V, C, M](implicit tfv : TwoFaceAny[F, V], ct  : CompileTime[C], di : DummyImplicit) : Chk[F, V, C, M] = create(tfv)
//  implicit def apply[F, V, C, M](tfv : TwoFaceAny[F, V])(implicit ct  : CompileTime[C]) : Chk[F, V, C, M] = create(tfv)
//}