package singleton.twoface
import impl._

object Checked {
  type F1[_]
  type F2[_,_]
  type Shell1[Cond[_], Msg[_], Arg1, Arg1Wide] =
    impl.CheckedShell1[Cond, Msg, impl.CheckedShell1[F1,F1,_,_,_], Arg1, Arg1Wide]
  type Shell1Sym[Cond[_], Msg[_], Sym, Arg1, Arg1Wide] =
    impl.CheckedShell1[Cond, Msg, Sym, Arg1, Arg1Wide]
  type Shell2[Cond[_,_], Msg[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] =
    impl.CheckedShell2[Cond, Msg, impl.CheckedShell2[F2,F2,_,_,_,_,_], Arg1, Arg1Wide, Arg2, Arg2Wide]
  type Shell2Sym[Cond[_,_], Msg[_,_], Sym, Arg1, Arg1Wide, Arg2, Arg2Wide] =
    impl.CheckedShell2[Cond, Msg, Sym, Arg1, Arg1Wide, Arg2, Arg2Wide]
}

object Checked0Param {
  trait Char    extends Checked0ParamAny.Char.Alias
  trait Int     extends Checked0ParamAny.Int.Alias
  trait Long    extends Checked0ParamAny.Long.Alias
  trait Float   extends Checked0ParamAny.Float.Alias
  trait Double  extends Checked0ParamAny.Double.Alias
  trait String  extends Checked0ParamAny.String.Alias
  trait Boolean extends Checked0ParamAny.Boolean.Alias
}

object Checked1Param {
  trait Char    extends Checked1ParamAny.Char.Alias
  trait Int     extends Checked1ParamAny.Int.Alias
  trait Long    extends Checked1ParamAny.Long.Alias
  trait Float   extends Checked1ParamAny.Float.Alias
  trait Double  extends Checked1ParamAny.Double.Alias
  trait String  extends Checked1ParamAny.String.Alias
  trait Boolean extends Checked1ParamAny.Boolean.Alias
}
