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

  type I = "$I"
  type Prime = "$Prime"
  type IsPrime[Num] =
  ITE[
    Num <= 1,
    false,
      (Prime := true) ==>
      (I := 2) ==>
      While[
        (I * I <= Num) && Prime,
          (Prime := (Num % I != 0)) ==>
          (I += 1),
        Prime
      ]
  ]

  def demoPrime[Num <: XInt](implicit op : IsPrime[Num]): op.Out {} = op.value
  val ex1 : false = demoPrime[1]
  val ex2 : true = demoPrime[2]
  val ex3 : true = demoPrime[3]
  val ex4 : false = demoPrime[4]
  val ex5 : true = demoPrime[5]

  def incPrime[Num <: XInt](implicit op : Num + 1, check : Require[IsPrime[Num]]) : op.Out {} = op.value
  val inc5 : 6 = incPrime[5]
//  val inc4 : 5 = incPrime[4] //Error:(41, 26) could not find implicit value for parameter check: singleton.ops.Require[singleton.ops.VarSpec.IsPrime[4]]

  def isPrime(num : Int) : Boolean = {
    if (num <= 1) return false
    var i = 2
    var prime = true
    while (i * i <= num && prime) {
      prime = num % i != 0
      i += 1
    }
    prime
  }
}