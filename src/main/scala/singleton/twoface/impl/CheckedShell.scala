package singleton.twoface.impl

import singleton.ops._
import singleton.twoface._


trait CheckedShell1[Cond[_], Msg[_], Sym, Arg1, Arg1Wide] {
  def unsafeCheck(arg1 : => Arg1Wide) : Unit
}

object CheckedShell1 {
  type F[_]
  implicit def ev[Cond[_], Msg[_], Sym, Arg1, Arg1Wide](
    implicit
    cond : TwoFace.Boolean.Shell1[Cond, Arg1, Arg1Wide],
    msg : TwoFace.String.Shell1[Msg, Arg1, Arg1Wide],
    req : TwoFace.Boolean.RequireShell[Cond[Arg1], Msg[Arg1], ITE[IsNonLiteral[Sym], GetType[Sym], GetType[CheckedShell1[F,F,_,_,_]]]]
  ) : CheckedShell1[Cond, Msg, Sym, Arg1, Arg1Wide] = new CheckedShell1[Cond, Msg, Sym, Arg1, Arg1Wide] {
    def unsafeCheck(arg1: => Arg1Wide): Unit = req(cond(arg1).getValue, msg(arg1).getValue)
  }
}


trait CheckedShell2[Cond[_,_], Msg[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] {
  def unsafeCheck(arg1 : => Arg1Wide, arg2 : => Arg2Wide) : Unit
}

object CheckedShell2 {
  type F[_,_] //Just a filler
  //Redirecting error message to shell
  type RequireRedir[Cond, Msg] = RequireMsgSym[Cond, Msg, CheckedShell2[F, F, _, _, _, _]]
  implicit def ev[Cond[_,_], Msg[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide](
    implicit
    cond : TwoFace.Boolean.Shell2[Cond, Arg1, Arg1Wide, Arg2, Arg2Wide],
    msg : TwoFace.String.Shell2[Msg, Arg1, Arg1Wide, Arg2, Arg2Wide],
    req : TwoFace.Boolean.Shell2[RequireRedir, Cond[Arg1, Arg2], Boolean, Msg[Arg1, Arg2], String]
  ) : CheckedShell2[Cond, Msg, Arg1, Arg1Wide, Arg2, Arg2Wide] = new CheckedShell2[Cond, Msg, Arg1, Arg1Wide, Arg2, Arg2Wide] {
    def unsafeCheck(arg1: => Arg1Wide, arg2 : => Arg2Wide): Unit =
      req(cond(arg1, arg2).getValue, msg(arg1, arg2).getValue)
  }
}
