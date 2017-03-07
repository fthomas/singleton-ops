# singleton-ops
[![Build Status](https://travis-ci.org/fthomas/singleton-ops.svg?branch=master)](https://travis-ci.org/fthomas/singleton-ops)
[![codecov](https://codecov.io/gh/fthomas/singleton-ops/branch/master/graph/badge.svg)](https://codecov.io/gh/fthomas/singleton-ops)
[![Join the chat at https://gitter.im/fthomas/singleton-ops](https://badges.gitter.im/fthomas/singleton-ops.svg)](https://gitter.im/fthomas/singleton-ops?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Scaladex](https://index.scala-lang.org/fthomas/singleton-ops/singleton-ops/latest.svg?color=blue)](https://index.scala-lang.org/fthomas/singleton-ops)

This library provides type-level operations for [Typelevel Scala][typelevel-scala] with [SIP-23][sip-23].
##Simple example:
```scala
import singleton.ops._

class MyVec[L] {
  def doubleSize = new MyVec[2 * L]
  def nSize[N] = new MyVec[N * L]
  def getLength(implicit length : SafeInt[L]) : Int = length
}
object MyVec {
  implicit def apply[L](implicit check : Require[L > 0]) : MyVec[L] = new MyVec[L]()
}
val myVec : MyVec[10] = MyVec[4 + 1].doubleSize
val myBadVec = MyVec[-1] //fails compilation, as required
```

---
## Using singleton-ops

The latest version of the library is 0.0.4, which is available for Typelevel Scala versions 2.11.8 & 2.12.1.

If you're using sbt, add the following to your build:

```sbt
libraryDependencies ++= Seq(
  "eu.timepit" %% "singleton-ops" % "0.0.4"
)
```
**Be sure to follow Typelevel Scala [instructions][typelevel-scala-use], to be able to use literal types in your code.** 

---
####Supported types:
* `Char with Singleton` (aliased as `XChar`) 
* `Int with Singleton` (aliased as `XInt`) 
* `Long with Singleton` (aliased as `XLong`)
* `Float with Singleton` (aliased as `XFloat`)
* `Double with Singleton` (aliased as `XDouble`)
* `String with Singleton` (aliased as `XString`)
* `Boolean with Singleton` (aliased as `XBoolean`)
* `Nat` (from [Shapeless][shapeless])

####Supported arithmetic operations:
* `type +[P1, P2]`          
* `type -[P1, P2]`          
* `type *[P1, P2]`          
* `type /[P1, P2]`          
* `type %[P1, P2]`          
* `type Abs[P1]`            
* `type Negate[P1]`         

####Supported relational operations:
* `type ==[P1, P2]`         
* `type !=[P1, P2]`         
* `type >[P1, P2]`          
* `type <[P1, P2]`          
* `type >=[P1, P2]`         
* `type <=[P1, P2]`         
* `type Min[P1, P2]`        
* `type Max[P1, P2]`        

####Supported logical operations:
* `type &&[P1, P2]`         
* `type ||[P1, P2]`         
* `type ![P1]`              

####Supported explicit conversion operations:
* `type ToNat[P1]`          
* `type ToChar[P1]`          
* `type ToInt[P1]`          
* `type ToLong[P1]`         
* `type ToFloat[P1]`          
* `type ToDouble[P1]`       
* `type ToString[P1]`          

####Supported string operations:
* `type +[P1, P2]` (concat)          
* `type Reverse[P1]`        
* `type Substring[P1, P2]`  

####Supported constraints operations:
* `type Require[P1]`        

####Supported control operations:
* `type ==>[A, B]` (`first A then B`)        
* `type ITE[I,T,E]` (`If (I) Then (T) Else (E)`)      
* `type While[Cond, Body, Ret]`  (`While (Cond) Run (Body) and then Return (Ret)`)      

####Supported assignment operations:
* `type :=[Name, Value]`        
* `type +=[Name, Value]`        
* `type -=[Name, Value]`        
* `type *=[Name, Value]`        
* `type /=[Name, Value]`        

####Supported Aux Pattern interface:
* `type OpAuxNat[O <: Op,      Ret_Out <: Nat]`
* `type OpAuxChar[O <: Op,     Ret_Out <: XChar]`     
* `type OpAuxInt[O <: Op,      Ret_Out <: XInt]`      
* `type OpAuxLong[O <: Op,     Ret_Out <: XLong]`    
* `type OpAuxFloat[O <: Op,    Ret_Out <: XFloat]`   
* `type OpAuxDouble[O <: Op,   Ret_Out <: XDouble]`  
* `type OpAuxString[O <: Op,   Ret_Out <: XString]`  
* `type OpAuxBoolean[O <: Op,  Ret_Out <: XBoolean]` 


## Examples

* `Int` type operations:
```scala
import singleton.ops._
def demo[L <: XInt](implicit p : L*L + L) : p.Out {} = p.value
val b : 30 = demo[5]
```
* `Long` type operations:
```scala
import singleton.ops._
def demoLong[L1 <: XLong, L2 <: XLong](implicit p : Min[L1*L1, L2+L2]) : p.Out {} = p.value
val bLong1 : 1L = demoLong[1L, 5L]
val bLong2 : 6L = demoLong[3L, 3L]
```

* `Double` type operations:
```scala
import singleton.ops._
def demoDouble[L1 <: XDouble, L2 <: XDouble](implicit p : L1 / L2 + 1.0) : p.Out {} = p.value
val bDouble : 1.2 = demoDouble[1.0, 5.0]
```

* Combined `Long` and `Int` type operations:
```scala
import singleton.ops._
def demoSumLongInt[L1 <: XLong, L2 <: XInt](implicit p : L1 + L2) : p.Out {} = p.value
val bSumLongInt : 16L = demoSumLongInt[8L, 8]
```

* `String` type operations:
```scala
import singleton.ops._
def demoString[P1 <: XString](implicit op : Reverse[P1] + P1) : op.Out {} = op.value
val bString : "cbaabc" = demoString["abc"]
```

* `Boolean` type operations:
```scala
import singleton.ops._
def demoBoolean[P1 <: XInt](implicit op : P1 < 0) : op.Out{} = op.value
val bBoolean1 : true = demoBoolean[-5]
val bBoolean2 : false = demoBoolean[5]
val bBoolean3 : false = demoBoolean[0]
```

* `Boolean` type constraints:
```scala
import singleton.ops._
def demoRequire[P1 <: XInt](implicit op : Require[P1 < 0]) : op.Out{} = op.value
scala> demoRequire[-1]
demoRequire[-1]
res0: Boolean(true) = true
scala> demoRequire[1]
<console>:16: error: could not find implicit value for parameter op: singleton.ops.Require[singleton.ops.<[1,0]]
       demoRequire[1]
```

* Shapeless' `Nat` type operations:
```scala
import singleton.ops._
import shapeless._
val n = Nat(5)
//Converting Nat to Int singleton occurs implicitly
def demoNatToSing[L <: Nat](implicit p : L+L) : p.Out {} = p.value
val bSing10 : 10 = demoNatToSing[n.N]
//Converting Int singleton to Nat requires explicit `ToNat`
def demoSingToNat[L <: XInt](implicit op : ToNat[L+L]) : op.Out = op.value
val bNat10 : shapeless.nat._10 = demoSingToNat[5]
```

* Working with large numbers doesn't slay the compiler:
```scala
import singleton.ops._
def bigMul[L1 <: XLong, L2 <: XLong](implicit p : L1 * L2) : p.Out {} = p.value
scala> bigMul[32000L, 6400000L]
res2: Long = 204800000000
```

## Contributors and participation

* [Frank S. Thomas](https://github.com/fthomas)
* [Matthew Pocock](https://github.com/drdozer)
* [Naoki Aoyama](https://github.com/aoiroaoino)
* [Oron Port](https://github.com/soronpo)

The singleton-ops project supports the [Typelevel][typelevel]
[code of conduct][typelevel-coc] and wants all of its channels (Gitter,
GitHub, etc.) to be welcoming environments for everyone.

[shapeless]: https://github.com/milessabin/shapeless
[sip-23]: http://docs.scala-lang.org/sips/pending/42.type.html
[typelevel]: http://typelevel.org/
[typelevel-coc]: http://typelevel.org/conduct.html
[typelevel-scala]: https://github.com/typelevel/scala
[typelevel-scala-use]: https://github.com/typelevel/scala#how-to-use-typelevel-scala
