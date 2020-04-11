package singleton.ops

import org.scalacheck.Properties
import shapeless.test.illTyped
import singleton.TestUtils._

class StringOpsSpec extends Properties("StringOps") {
  property("Substring[foobar, 3] == bar") = wellTyped {
    def f[S <: XString, I <: XInt](implicit op : Substring[S, I]) : op.Out{} = op.value
    val r : W.`"bar"`.T = f[W.`"foobar"`.T, W.`3`.T]
  }
  property("Substring abort") = wellTyped {illTyped("""implicitly[Substring[W.`"abc"`.T, W.`4`.T]]""")}
  property("Substring unsupported") = wellTyped {illTyped("""implicitly[Substring[W.`true`.T, W.`2`.T]]""")}

  property("SubSequence[foobar, 1, 4] == oob") = wellTyped {
    def f[S <: XString, B <: XInt, E <: XInt](implicit op : SubSequence[S, B, E]) : op.Out{} = op.value
    val r : W.`"oob"`.T = f[W.`"foobar"`.T, W.`1`.T, W.`4`.T]
  }
  property("SubSequence abort") = wellTyped {illTyped("""implicitly[SubSequence[W.`"abc"`.T, W.`5`.T, W.`7`.T]]""")}
  property("SubSequence unsupported") = wellTyped {illTyped("""implicitly[SubSequence[W.`true`.T, W.`2`.T, W.`3`.T]]""")}

  property("StartsWith[foobar, foo] == true") = wellTyped {
    def f[S <: XString, Pre <: XString](implicit op : StartsWith[S, Pre]) : op.Out{} = op.value
    val r : W.`true`.T = f[W.`"foobar"`.T, W.`"foo"`.T]
  }
  property("StartsWith unsupported") = wellTyped {illTyped("""implicitly[StartsWith[W.`"foobar"`.T, W.`true`.T]]""")}

  property("EndsWith[foobar, bar] == true") = wellTyped {
    def f[S <: XString, Suf <: XString](implicit op : EndsWith[S, Suf]) : op.Out{} = op.value
    val r : W.`true`.T = f[W.`"foobar"`.T, W.`"bar"`.T]
  }
  property("EndsWith unsupported") = wellTyped {illTyped("""implicitly[EndsWith[W.`"foobar"`.T, W.`true`.T]]""")}

  property("Head[foobar] == f") = wellTyped {
    def f[S <: XString](implicit op : Head[S]) : op.Out{} = op.value
    val r : W.`'f'`.T = f[W.`"foobar"`.T]
  }
  property("Head abort") = wellTyped {illTyped("""implicitly[Head[W.`""`.T]]""")}
  property("Head unsupported") = wellTyped {illTyped("""implicitly[Head[W.`0`.T]]""")}

  property("Tail[foobar] == oobar") = wellTyped {
    def f[S <: XString](implicit op : Tail[S]) : op.Out{} = op.value
    val r : W.`"oobar"`.T = f[W.`"foobar"`.T]
  }
  property("Tail unsupported") = wellTyped {illTyped("""implicitly[Tail[W.`0`.T]]""")}

  property("CharAt[foobar, 3] == b") = wellTyped {
    def f[S <: XString, I <: XInt](implicit op : CharAt[S, I]) : op.Out{} = op.value
    val r : W.`'b'`.T = f[W.`"foobar"`.T, W.`3`.T]
  }
  property("CharAt abort") = wellTyped {illTyped("""implicitly[CharAt[W.`"abc"`.T, W.`5`.T]]""")}
  property("CharAt unsupported") = wellTyped {illTyped("""implicitly[CharAt[W.`true`.T, W.`2`.T]]""")}

  property("Length[foobar] == 6") = wellTyped {
    def f[S <: XString](implicit op : Length[S]) : op.Out{} = op.value
    val r : W.`6`.T = f[W.`"foobar"`.T]
  }
  property("Length unsupported") = wellTyped {illTyped("""implicitly[Length[W.`true`.T]]""")}

  property("Matches[foobar, fo+.*] == true") = wellTyped {
    def f[S <: XString, Regex <: XString](implicit op : Matches[S, Regex]) : op.Out{} = op.value
    val r : W.`true`.T = f[W.`"foobar"`.T, W.`"fo+.*"`.T]
  }
  property("Matches abort") = wellTyped {illTyped("""implicitly[Matches[W.`"abc"`.T, W.`"[a"`.T]]""")}
  property("Matches unsupported") = wellTyped {illTyped("""implicitly[Matches[W.`"foobar"`.T, W.`true`.T]]""")}

  property("FirstMatch[foobar, b[ar]+] == bar") = wellTyped {
    def f[S <: XString, Regex <: XString](implicit op : FirstMatch[S, Regex]) : op.Out{} = op.value
    val r : W.`"bar"`.T = f[W.`"foobar"`.T, W.`"b[ar]+"`.T]
  }
  property("FirstMatch abort") = wellTyped {illTyped("""implicitly[FirstMatch[W.`"abc"`.T, W.`"[a"`.T]]""")}
  property("FirstMatch unsupported") = wellTyped {illTyped("""implicitly[FirstMatch[W.`0`.T, W.`".*"`.T]]""")}

  property("PrefixMatch[foobar, fo+] == foo") = wellTyped {
    def f[S <: XString, Regex <: XString](implicit op : PrefixMatch[S, Regex]) : op.Out{} = op.value
    val r : W.`"foo"`.T = f[W.`"foobar"`.T, W.`"fo+"`.T]
  }
  property("PrefixMatch abort") = wellTyped {illTyped("""implicitly[PrefixMatch[W.`"abc"`.T, W.`"[a"`.T]]""")}
  property("PrefixMatch unsupported") = wellTyped {illTyped("""implicitly[PrefixMatch[W.`true`.T, W.`".*"`.T]]""")}

  property("ReplaceFirstMatch[foobar, [oa], z] == fzobar") = wellTyped {
    def f[S <: XString, Regex <: XString, R <: XString](implicit op : ReplaceFirstMatch[S, Regex, R]) : op.Out{} = op.value
    val r : W.`"fzobar"`.T = f[W.`"foobar"`.T, W.`"[oa]"`.T, W.`"z"`.T]
  }
  property("ReplaceFirstMatch abort") = wellTyped {illTyped("""implicitly[ReplaceFirstMatch[W.`"abc"`.T, W.`"[a"`.T, W.`"z"`.T]]""")}
  property("ReplaceFirstMatch unsupported") = wellTyped {illTyped("""implicitly[ReplaceFirstMatch[W.`0`.T, W.`".*"`.T, W.`"z"`.T]]""")}

  property("ReplaceAllMatches[foobar, [oa], z] == fzzbzr") = wellTyped {
    def f[S <: XString, Regex <: XString, R <: XString](implicit op : ReplaceAllMatches[S, Regex, R]) : op.Out{} = op.value
    val r : W.`"fzzbzr"`.T = f[W.`"foobar"`.T, W.`"[oa]"`.T, W.`"z"`.T]
  }  
  property("ReplaceAllMatches abort") = wellTyped {illTyped("""implicitly[ReplaceAllMatches[W.`"abc"`.T, W.`"[a"`.T, W.`"z"`.T]]""")}
  property("ReplaceAllMatches unsupported") = wellTyped {illTyped("""implicitly[ReplaceAllMatches[W.`0`.T, W.`".*"`.T, W.`"z"`.T]]""")}
}
