package singleton.twoface.impl

import singleton.twoface._


trait CheckedShell1[Cond[_], Msg[_], Sym, Arg1, Arg1Wide] {
  def unsafeCheck(arg1 : => Arg1Wide) : Unit
}

object CheckedShell1 {
  implicit def ev[Cond[_], Msg[_], Sym, Arg1, Arg1Wide](
    implicit
    cond : TwoFace.Boolean.Shell1[Cond, Arg1, Arg1Wide],
    msg : TwoFace.String.Shell1[Msg, Arg1, Arg1Wide],
    req : TwoFace.Boolean.RequireShell[Cond[Arg1], Msg[Arg1], Sym]
  ) : CheckedShell1[Cond, Msg, Sym, Arg1, Arg1Wide] =
    new CheckedShell1[Cond, Msg, Sym, Arg1, Arg1Wide] {
      def unsafeCheck(arg1: => Arg1Wide): Unit = req(cond(arg1).getValue, msg(arg1).getValue)
    }
}


trait CheckedShell2[Cond[_,_], Msg[_,_], Sym, Arg1, Arg1Wide, Arg2, Arg2Wide] {
  def unsafeCheck(arg1 : => Arg1Wide, arg2 : => Arg2Wide) : Unit
}

object CheckedShell2 {
  implicit def ev[Cond[_,_], Msg[_,_], Sym, Arg1, Arg1Wide, Arg2, Arg2Wide](
    implicit
    cond : TwoFace.Boolean.Shell2[Cond, Arg1, Arg1Wide, Arg2, Arg2Wide],
    msg : TwoFace.String.Shell2[Msg, Arg1, Arg1Wide, Arg2, Arg2Wide],
    req : TwoFace.Boolean.RequireShell[Cond[Arg1, Arg2], Msg[Arg1, Arg2], Sym]
  ) : CheckedShell2[Cond, Msg, Sym, Arg1, Arg1Wide, Arg2, Arg2Wide] =
    new CheckedShell2[Cond, Msg, Sym, Arg1, Arg1Wide, Arg2, Arg2Wide] {
      def unsafeCheck(arg1: => Arg1Wide, arg2 : => Arg2Wide): Unit =
        req(cond(arg1, arg2).getValue, msg(arg1, arg2).getValue)
    }
}
