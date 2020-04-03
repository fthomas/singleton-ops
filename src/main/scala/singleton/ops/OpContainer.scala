package singleton.ops

object OpContainer {
  //Create an equivalence implicit rule with this `Eq` type
  //F - from type
  //T - to type
  //Wide - the T and F wide representation
  type Eq[F, T, Wide] = Require[ITE[IsNonLiteral[T], ImplicitFound[T =:= Wide], F == T]]

  //For an op container with a single argument, extend the companion object using this trait.
  //For example:
  //class Vec[Size]
  //object Vec extends OpContainer.Eq1[Vec, Int]
  //val v1 : Vec[2] = new Vec[1 + 1]
  //val vInt : Vec[Int] = new Vec[1 + 1]
  trait Eq1[C[F1], Wide1] {
    implicit def argCast[F1,T1](c : C[F1])(implicit eq : Eq[F1,T1,Wide1]) : C[T1] = c.asInstanceOf[C[T1]]
  }
}
