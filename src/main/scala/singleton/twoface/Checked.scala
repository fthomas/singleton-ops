package singleton.twoface

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
