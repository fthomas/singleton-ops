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

trait CheckedShell1Builder[ChkShl[_,_], Cond[_], Msg[_], Arg1Wide] {
  def create[Sym, Arg1](_unsafeCheck : Arg1Wide => Unit) : ChkShl[Sym, Arg1]
  implicit def ev[Sym, Arg1](
    implicit
    cond : TwoFace.Boolean.Shell1[Cond, Arg1, Arg1Wide],
    msg : TwoFace.String.Shell1[Msg, Arg1, Arg1Wide],
    req : TwoFace.Boolean.RequireShell[Cond[Arg1], Msg[Arg1], Sym]
  ) : ChkShl[Sym, Arg1] = create[Sym, Arg1](arg1 => req(cond(arg1).getValue, msg(arg1).getValue))
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

trait CheckedShell2Builder[ChkShl[_,_,_], Cond[_,_], Msg[_,_], Arg1Wide, Arg2Wide] {
  def create[Sym, Arg1, Arg2](_unsafeCheck : (Arg1Wide, Arg2Wide) => Unit) : ChkShl[Sym, Arg1, Arg2]
  implicit def ev[Sym, Arg1, Arg2](
    implicit
    cond : TwoFace.Boolean.Shell2[Cond, Arg1, Arg1Wide, Arg2, Arg2Wide],
    msg : TwoFace.String.Shell2[Msg, Arg1, Arg1Wide, Arg2, Arg2Wide],
    req : TwoFace.Boolean.RequireShell[Cond[Arg1, Arg2], Msg[Arg1, Arg2], Sym]
  ) : ChkShl[Sym, Arg1, Arg2] = create[Sym, Arg1, Arg2]((arg1, arg2) => req(cond(arg1, arg2).getValue, msg(arg1, arg2).getValue))
}
