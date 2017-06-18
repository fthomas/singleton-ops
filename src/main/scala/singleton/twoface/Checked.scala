package singleton.twoface

import scala.meta._
import scala.collection.immutable.Seq

class checked0Param[Cond[_], Msg[_], TFace] extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val q"type T = $tFaceName" = q"type T = $TFace"
        val updatedCls =
          q"""
             final class ${Type.Name(name.value)}[T] (val value : $TFace) extends AnyVal with
               _root_.singleton.twoface.impl.Checked0Param[$TFace, T] with
               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString())}[T] {
               @inline def getValue : $TFace = value
             }
           """
        val companion =
          q"""
             object ${Term.Name(name.value)} extends _root_.singleton.twoface.impl.Checked0Param.Builder[${Type.Name(name.value)}, $Cond, $Msg, $TFace]
           """
        Term.Block(Seq(updatedCls, companion))
      case _ =>
        println(defn.structure)
        abort("@checked must annotate a class.")
    }
  }
}

class checked1Param[Cond[_,_], Msg[_,_], TFace, ParamFace] extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val q"type T = $tFaceName" = q"type T = $TFace"
        val updatedCls =
          q"""
             final class ${Type.Name(name.value)}[T, Param] (val value : $TFace) extends AnyVal with
               _root_.singleton.twoface.impl.Checked1Param[$TFace, T] with
               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString())}[T] {
               @inline def getValue : $TFace = value
             }
           """
        val companion =
          q"""
             object ${Term.Name(name.value)} extends _root_.singleton.twoface.impl.Checked1Param.Builder[${Type.Name(name.value)}, $Cond, $Msg, $TFace, $ParamFace]
           """
        Term.Block(Seq(updatedCls, companion))
      case _ =>
        println(defn.structure)
        abort("@checked must annotate a class.")
    }
  }
}


