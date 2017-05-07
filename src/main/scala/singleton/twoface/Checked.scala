package singleton.twoface

/**
  * Created by soronpo on 07/05/2017.
  */
import singleton.ops._
//import TwoFace._
object Checked {
  trait Builder[TF[_,C[_],_], Face] {
    def apply[T, Cond[_], Msg](value : Face) : TF[T, Cond, Msg]
    implicit def ev[T, Cond[_], Msg](implicit v : Id[T], ct : CompileTime[Cond[T]]) :
      TF[T, Cond, Msg] = apply[T, Cond, Msg](v.value.asInstanceOf[Face])
//    implicit def conv[T <: Face, Cond[_], Msg](t : T)(implicit fb : FallBack[Face, T], ct : CompileTime[Cond[T]]) :
//      TF[T, Cond, Msg] = apply[T, Cond, Msg](if (fb.isLiteral) fb.value.get else t)
      implicit def unsafe[T <: Face, Cond[_], Msg](t : T)(implicit ct : CompileTime[Cond[T]], rt : RunTime[T]) :
      TF[Face, Cond, Msg] = apply[Face, Cond, Msg](t)
      implicit def safe[T <: Face with Singleton, Cond[_], Msg](t : T)(implicit ct : CompileTime[Cond[T]]) :
      TF[T, Cond, Msg] = apply[T, Cond, Msg](t)
  }

  @scala.annotation.implicitNotFound("${Msg}")
  final class Int[T, Cond[_], Msg] private(val value : scala.Int) extends AnyVal with TwoFaceAny.Int[T] {
    @inline def getValue : scala.Int = value
  }
  object Int extends Builder[Int, scala.Int] {
    def apply[T, Cond[_], Msg](value : scala.Int) : Int[T, Cond, Msg] = new Int[T, Cond, Msg](value)
  }
}


//object Bla {
//  val a : Checked.Int[5, true, "df"] = Checked.Int.conv(5)
//  implicitly[Checked.Int[5, true, "df"]]
//}
//trait CheckedObj[Chk[F, V, C, M] <: Checked[F, V, C, M]] {
//  def create[F, V, C, M](tfv : TwoFaceAny[F, V]) : Chk[F, V, C, M]
//  implicit def ev[F, V, C, M](implicit tfv : TwoFaceAny[F, V], ct  : CompileTime[C], di : DummyImplicit) : Chk[F, V, C, M] = create(tfv)
//  implicit def apply[F, V, C, M](tfv : TwoFaceAny[F, V])(implicit ct  : CompileTime[C]) : Chk[F, V, C, M] = create(tfv)
//}