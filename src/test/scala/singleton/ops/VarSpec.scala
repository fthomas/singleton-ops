package singleton.ops

object VarSpec {
  type Average[B,W] = SV["Best",B] ==> SV["Worst", W] ==> (GV["Best"] + GV["Worst"]) / 2
  def demoSV[B <: XInt, W <: XInt](implicit op: Average[B, W]): op.Out {} = op.value
  val bSV1 : 6 = demoSV[9,3]

  type LoopDemo[M] =
    ("$Cnt" := 0) ==>
    ("$Result" := 0) ==>
      While["$Cnt" <= M, ("$Result" += "$Cnt") ==> ("$Cnt" += 1), "$Result"]

    def demoLoop[M <: XInt](implicit op: LoopDemo[M]): op.Out {} = op.value
    val bLoop1 : 10 = demoLoop[4]

}