# singleton-ops

[![Join the chat at https://gitter.im/fthomas/singleton-ops](https://badges.gitter.im/fthomas/singleton-ops.svg)](https://gitter.im/fthomas/singleton-ops?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/fthomas/singleton-ops.svg?branch=master)](https://travis-ci.org/fthomas/singleton-ops)
[![codecov](https://codecov.io/gh/fthomas/singleton-ops/branch/master/graph/badge.svg)](https://codecov.io/gh/fthomas/singleton-ops)

This is a proof of concept library that provides operations for primitive
and `String` singleton types. It lifts value-level operations of these types
to the type-level and allows for example to add `Double`s at the type-level.

## Examples

* The following adds the singleton types `Double(3.6)` and `Double(4.9)`
and yields the type `Double(8.5)` as result:
```scala
scala> val p = Plus[3.6, 4.9]
p: singleton.ops.Plus[3.6,4.9]{type Out = Double(8.5)} = $anon$1@72b03361

scala> 8.5 : p.Out
res0: p.Out = 8.5

scala> 8.6 : p.Out
<console>:16: error: type mismatch;
 found   : Double(8.6)
 required: p.Out
    (which expands to)  Double(8.5)
       8.6 : p.Out
       ^
```
Note that the types are [literal-based singleton types][sip-23].

* Working with large numbers doesn't slay the compiler:
```scala
scala> Times[32000L, 6400000L]
res2: singleton.ops.Times[32000L,6400000L]{type Out = Long(204800000000L)} = $anon$1@33a50777
```

* This `concat` method concatenates two `String`s both at the value- and
type-level (the result type depends on the arguments):
```scala
scala> def concat(s1: String, s2: String)(implicit c: Concat[s1.type, s2.type]) = c.value
concat: (s1: String, s2: String)(implicit c: singleton.ops.Concat[s1.type,s2.type])c.Out

scala> concat("abc", "xyz")
res1: String("abcxyz") = abcxyz
```

* We can compose operations:
```scala
scala> def concatReverse[A](s1: String, s2: String)(implicit c: Concat.Aux[s1.type, s2.type, A], r: Reverse[A]) = r.value
concatReverse: [A](s1: String, s2: String)(implicit c: singleton.ops.Concat.Aux[s1.type,s2.type,A], implicit r: singleton.ops.Reverse[A])r.Out

scala> concatReverse("123", "abc")
res2: String("cba321") = cba321
```

[singleton-types]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#singleton-typed-literals
[sip-23]: http://docs.scala-lang.org/sips/pending/42.type.html
