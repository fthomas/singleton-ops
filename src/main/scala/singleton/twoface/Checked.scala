package singleton.twoface

import scala.meta._
import scala.collection.immutable.Seq

class checked0Param[Cond[_], Msg[_], TFace] extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val q"type T = $tFaceName" = q"type T = $TFace"
        val chkNameStr = name.value
        val shellNameStr = chkNameStr + "Shell"
        val chkTypeName = Type.Name(chkNameStr)
        val chkTermName = Term.Name(chkNameStr)
        val chkCtorName = Ctor.Name(chkNameStr)
        val shellClassName = Type.Name(shellNameStr)
        val shellCtorName = Ctor.Name(shellNameStr)
        val shellObjectName = Term.Name(shellNameStr)
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
               type Shell[T] = $shellClassName[T]
             }
           """
        val shellCls =
          q"""
             final class $shellClassName[T] extends
               _root_.singleton.twoface.impl.Checked0ParamShell[$chkTypeName, $TFace, T] {
               def apply(value : $TFace) : $chkTypeName[T] = new $chkCtorName[T](value)
             }
           """
        val shellCompanion =
          q"""
             object $shellObjectName extends _root_.singleton.twoface.impl.Checked0ParamShell.Builder[$shellClassName, $chkTypeName, $Cond, $Msg, $TFace] {
               def create[T] : $shellClassName[T] = new $shellCtorName[T]
             }
           """
        val termBlock = Term.Block(Seq(updatedCls, companion, shellCls, shellCompanion))
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
        val shellNameStr = chkNameStr + "Shell"
        val chkTypeName = Type.Name(chkNameStr)
        val chkTermName = Term.Name(chkNameStr)
        val chkCtorName = Ctor.Name(chkNameStr)
        val shellClassName = Type.Name(shellNameStr)
        val shellCtorName = Ctor.Name(shellNameStr)
        val shellObjectName = Term.Name(shellNameStr)
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
               type Shell[T, Param] = $shellClassName[T, Param]
             }
           """
        val shellCls =
          q"""
             final class $shellClassName[T, Param] extends
               _root_.singleton.twoface.impl.Checked1ParamShell[$chkTypeName, $TFace, T, Param] {
               def apply(value : $TFace) : $chkTypeName[T, Param] = new $chkCtorName[T, Param](value)
             }
           """
        val shellCompanion =
          q"""
             object $shellObjectName extends _root_.singleton.twoface.impl.Checked1ParamShell.Builder[$shellClassName, $chkTypeName, $Cond, $Msg, $TFace, $ParamFace] {
               def create[T, Param] : $shellClassName[T, Param] = new $shellCtorName[T, Param]
             }
           """
        val termBlock = Term.Block(Seq(updatedCls, companion, shellCls, shellCompanion))
        //        print(termBlock)
        termBlock
      case _ =>
        //        println(defn.structure)
        abort("@checked must annotate a class.")
    }
  }
}
