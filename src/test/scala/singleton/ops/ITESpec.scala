package singleton.ops

object ITESpec {
  type MyIfFunc[P1] = ITE[P1 < 0, 5, 7]
  def demoITE[P1 <: XInt](implicit op: MyIfFunc[P1]): op.Out {} = op.value
  val bITE1 : 7 = demoITE[1]
  val bITE2 : 5 = demoITE[-1]
}
