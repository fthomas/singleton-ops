package singleton

package object twoface {
  @inline implicit def fromTwoFaceUnsafe[Face, T <: Face](tf : TwoFace.TwoFaceAny[Face, T]) : Face = tf.getValue
  @inline implicit def fromTwoFaceSafe[Face, T <: Face with Singleton](tf : TwoFace.TwoFaceAny[Face, T])
    (implicit sc: ValueOf[T]) : T = valueOf[T]

}
