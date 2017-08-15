package singleton.twoface

import scala.meta._
import scala.collection.immutable.Seq

object Checked {
  type Shell1[Cond[_], Msg[_], Arg1, Arg1Wide] =
    impl.CheckedShell1[Cond, Msg, Arg1, Arg1Wide]
  type Shell2[Cond[_,_], Msg[_,_], Arg1, Arg1Wide, Arg2, Arg2Wide] =
    impl.CheckedShell2[Cond, Msg, Arg1, Arg1Wide, Arg2, Arg2Wide]
}

class checked0Param[Cond[_], Msg[_], TFace] extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val q"type T = $tFaceName" = q"type T = $TFace"
        val chkNameStr = name.value
        val chkTypeName = Type.Name(chkNameStr)
        val chkTermName = Term.Name(chkNameStr)
        val updatedCls =
          q"""
             final class $chkTypeName[T] (val value : $TFace) extends AnyVal with
               _root_.singleton.twoface.impl.Checked0Param[$chkTypeName, $Cond, $Msg, $TFace, T] with
               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString())}[T] {
               @inline def getValue : $TFace = value
             }
           """
        val companion =
          q"""
             object $chkTermName extends _root_.singleton.twoface.impl.Checked0Param.Builder[$chkTypeName, $Cond, $Msg, $TFace] {
             }
           """
        val termBlock = Term.Block(Seq(updatedCls, companion))
//        print(termBlock)
        termBlock
      case _ =>
//        println(defn.structure)
        abort("@checked must annotate a class.")
    }
  }
}


class checked1Param[Cond[_,_], Msg[_,_], TFace, ParamFace] extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val q"type T = $tFaceName" = q"type T = $TFace"
        val chkNameStr = name.value
        val chkTypeName = Type.Name(chkNameStr)
        val chkTermName = Term.Name(chkNameStr)
        val updatedCls =
          q"""
             final class $chkTypeName[T, Param] (val value : $TFace) extends AnyVal with
               _root_.singleton.twoface.impl.Checked1Param[$chkTypeName, $Cond, $Msg, $TFace, T, $ParamFace, Param] with
               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString())}[T] {
               @inline def getValue : $TFace = value
             }
           """
        val companion =
          q"""
             object $chkTermName extends _root_.singleton.twoface.impl.Checked1Param.Builder[$chkTypeName, $Cond, $Msg, $TFace, $ParamFace] {
             }
           """
        val termBlock = Term.Block(Seq(updatedCls, companion))
        //        print(termBlock)
        termBlock
      case _ =>
        //        println(defn.structure)
        abort("@checked must annotate a class.")
    }
  }
}
