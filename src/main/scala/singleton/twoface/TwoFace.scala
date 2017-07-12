package singleton.twoface

import impl._

object TwoFace {
  type Char[T] = TwoFaceAny.Char.Aux[T]
  val Char = TwoFaceAny.Char

  type Int[T] = TwoFaceAny.Int.Aux[T]
  val Int = TwoFaceAny.Int

  type Long[T] = TwoFaceAny.Long.Aux[T]
  val Long = TwoFaceAny.Long

  type Float[T] = TwoFaceAny.Float.Aux[T]
  val Float = TwoFaceAny.Float

  type Double[T] = TwoFaceAny.Double.Aux[T]
  val Double = TwoFaceAny.Double

  type String[T] = TwoFaceAny.String.Aux[T]
  val String = TwoFaceAny.String

  type Boolean[T] = TwoFaceAny.Boolean.Aux[T]
  val Boolean = TwoFaceAny.Boolean

  ////////////////////////////////////////////////////////////////////////////////
  // Char
  ////////////////////////////////////////////////////////////////////////////////
  def apply[T <: scala.Char with Singleton](value : T)
  : Char[T] = TwoFace.Char.create[T](value)
  def apply(value : scala.Char)(
    implicit
    di1 : DummyImplicit
  ) : Char[scala.Char] = TwoFace.Char.create[scala.Char](value)
  ////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////
  // Int
  ////////////////////////////////////////////////////////////////////////////////
  def apply[T <: scala.Int with Singleton](value : T)(
    implicit
    di1 : DummyImplicit
  ) : Int[T] = TwoFace.Int.create[T](value)
  def apply(value : scala.Int)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
  ) : Int[scala.Int] = TwoFace.Int.create[scala.Int](value)
  ////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////
  // Long
  ////////////////////////////////////////////////////////////////////////////////
  def apply[T <: scala.Long with Singleton](value : T)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
  ) : Long[T] = TwoFace.Long.create[T](value)
  def apply(value : scala.Long)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
  ) : Long[scala.Long] = TwoFace.Long.create[scala.Long](value)
  ////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////
  // Float
  ////////////////////////////////////////////////////////////////////////////////
  def apply[T <: scala.Float with Singleton](value : T)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
  ) : Float[T] = TwoFace.Float.create[T](value)
  def apply(value : scala.Float)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
    di4 : DummyImplicit,
  ) : Float[scala.Float] = TwoFace.Float.create[scala.Float](value)
  ////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////
  // Double
  ////////////////////////////////////////////////////////////////////////////////
  def apply[T <: scala.Double with Singleton](value : T)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
    di4 : DummyImplicit,
  ) : Double[T] = TwoFace.Double.create[T](value)
  def apply(value : scala.Double)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
    di4 : DummyImplicit,
    di5 : DummyImplicit,
  ) : Double[scala.Double] = TwoFace.Double.create[scala.Double](value)
  ////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////
  // String
  ////////////////////////////////////////////////////////////////////////////////
  def apply[T <: java.lang.String with Singleton](value : T)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
    di4 : DummyImplicit,
    di5 : DummyImplicit,
  ) : String[T] = TwoFace.String.create[T](value)
  def apply(value : java.lang.String)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
    di4 : DummyImplicit,
    di5 : DummyImplicit,
    di6 : DummyImplicit,
  ) : String[java.lang.String] = TwoFace.String.create[java.lang.String](value)
  ////////////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////////////
  // Boolean
  ////////////////////////////////////////////////////////////////////////////////
  def apply[T <: scala.Boolean with Singleton](value : T)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
    di4 : DummyImplicit,
    di5 : DummyImplicit,
    di6 : DummyImplicit,
  ) : Boolean[T] = TwoFace.Boolean.create[T](value)
  def apply(value : scala.Boolean)(
    implicit
    di1 : DummyImplicit,
    di2 : DummyImplicit,
    di3 : DummyImplicit,
    di4 : DummyImplicit,
    di5 : DummyImplicit,
    di6 : DummyImplicit,
    di7 : DummyImplicit,
  ) : Boolean[scala.Boolean] = TwoFace.Boolean.create[scala.Boolean](value)
  ////////////////////////////////////////////////////////////////////////////////

}