# singleton-ops
[![Build Status](https://travis-ci.org/fthomas/singleton-ops.svg?branch=master)](https://travis-ci.org/fthomas/singleton-ops)
[![codecov](https://codecov.io/gh/fthomas/singleton-ops/branch/master/graph/badge.svg)](https://codecov.io/gh/fthomas/singleton-ops)

This is a proof of concept library that provides operations for primitive
and `String` singleton types.

```scala
scala> val p = Plus[W.`3.6`.T, W.`4.9`.T]
p: singleton.ops.Plus[Double(3.6),Double(4.9)]{type Out = Double(8.5)} = $anon$1@4e24b316

scala> 8.5: p.Out
res0: p.Out = 8.5

scala> 8.6: p.Out
<console>:16: error: type mismatch;
 found   : Double(8.6)
 required: p.Out
    (which expands to)  Double(8.5)
       8.6: p.Out
       ^
```
