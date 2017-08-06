package singleton.twoface

import scala.meta._
import scala.collection.immutable.Seq

class checked0Param[Cond[_], Msg[_], TFace] extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val q"type T = $tFaceName" = q"type T = $TFace"
        val primName = name.value
        val secName = "_" + name.value
        val shellName = name.value + "Shell"
        val traitName = name.value + "Like"
        val traitTypeName = Type.Name(traitName)
        val traitCtorName = Ctor.Name(traitName)
        val traitTermName = Term.Name(traitName)
        val primTypeName = Type.Name(primName)
        val primCtorName = Ctor.Name(primName)
        val primTermName = Term.Name(primName)
        val primTerm = Pat.Var.Term(primTermName)
        val secTypeName = Type.Name(secName)
        val secCtorName = Ctor.Name(secName)
        val shellClassName = Type.Name(shellName)
        val shellCtorName = Ctor.Name(shellName)
        val shellObjectName = Term.Name(shellName)
        val likeTrait =
          q"""
             trait $traitTypeName extends Any with
               _root_.singleton.twoface.impl.Checked0Param[$Cond, $Msg, $TFace] with
               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString() + "Like")} {
             }
           """
        val updatedCls =
          q"""
             final class $secTypeName[T0] (val value : $TFace) extends AnyVal with $traitCtorName {
               type T = T0
               @inline def getValue : $TFace = value
             }
           """
        val companion =
          q"""
             object $traitTermName extends _root_.singleton.twoface.impl.Checked0Param.Builder[$primTypeName, $secTypeName, $Cond, $Msg, $TFace] {
               type Shell[T] = $shellClassName[T]
             }
           """
        val typeBlock =
          q"""
             type $primTypeName[T0] = $traitTypeName {type T <: T0}
           """
        val valBlock =
          q"""
             val $primTerm = $traitTermName
           """
        val shellCls =
          q"""
             final class $shellClassName[T] extends
               _root_.singleton.twoface.impl.Checked0ParamShell[$primTypeName, $TFace, T] {
               def apply(value : $TFace) : $primTypeName[T] = new $secCtorName[T](value)
             }
           """
        val shellCompanion =
          q"""
             object $shellObjectName extends _root_.singleton.twoface.impl.Checked0ParamShell.Builder[$shellClassName, $primTypeName, $Cond, $Msg, $TFace] {
               def create[T] : $shellClassName[T] = new $shellCtorName[T]
             }
           """
        val termBlock = Term.Block(Seq(likeTrait, updatedCls, companion, typeBlock, valBlock, shellCls, shellCompanion))
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
        val primName = name.value
        val secName = "_" + name.value
        val shellName = name.value + "Shell"
        val traitName = name.value + "Like"
        val traitTypeName = Type.Name(traitName)
        val traitCtorName = Ctor.Name(traitName)
        val traitTermName = Term.Name(traitName)
        val primTypeName = Type.Name(primName)
        val primCtorName = Ctor.Name(primName)
        val primTermName = Term.Name(primName)
        val primTerm = Pat.Var.Term(primTermName)
        val secTypeName = Type.Name(secName)
        val secCtorName = Ctor.Name(secName)
        val shellClassName = Type.Name(shellName)
        val shellCtorName = Ctor.Name(shellName)
        val shellObjectName = Term.Name(shellName)
        val likeTrait =
          q"""
             trait $traitTypeName[Param] extends Any with
               _root_.singleton.twoface.impl.Checked1Param[$Cond, $Msg, $TFace, $ParamFace] with
               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString() + "Like")} {
             }
           """
        val updatedCls =
          q"""
             final class $secTypeName[T0, Param] (val value : $TFace) extends AnyVal with $traitCtorName[Param] {
               type T = T0
               @inline def getValue : $TFace = value
             }
           """
        val companion =
          q"""
             object $traitTermName extends _root_.singleton.twoface.impl.Checked1Param.Builder[$primTypeName, $secTypeName, $Cond, $Msg, $TFace, $ParamFace] {
               type Shell[T, Param] = $shellClassName[T, Param]
             }
           """
        val typeBlock =
          q"""
             type $primTypeName[T0, Param] = $traitTypeName[Param] {type T <: T0}
           """
        val valBlock =
          q"""
             val $primTerm = $traitTermName
           """
        val shellCls =
          q"""
             final class $shellClassName[T, Param] extends
               _root_.singleton.twoface.impl.Checked1ParamShell[$primTypeName, $TFace, T, Param] {
               def apply(value : $TFace) : $primTypeName[T, Param] = new $secCtorName[T, Param](value)
             }
           """
        val shellCompanion =
          q"""
             object $shellObjectName extends _root_.singleton.twoface.impl.Checked1ParamShell.Builder[$shellClassName, $primTypeName, $Cond, $Msg, $TFace, $ParamFace] {
               def create[T, Param] : $shellClassName[T, Param] = new $shellCtorName[T, Param]
             }
           """
        val termBlock = Term.Block(Seq(likeTrait, updatedCls, companion, typeBlock, valBlock, shellCls, shellCompanion))
        //        print(termBlock)
        termBlock
      case _ =>
        //        println(defn.structure)
        abort("@checked must annotate a class.")
    }
  }
}

//
//class checked1Param[Cond[_,_], Msg[_,_], TFace, ParamFace] extends scala.annotation.StaticAnnotation {
//  inline def apply(defn: Any): Any = meta {
//    defn match {
//      case cls @ Defn.Class(_, name, _, ctor, _) =>
//        val q"type T = $tFaceName" = q"type T = $TFace"
//        val className = Type.Name(name.value)
//        val objectName = Term.Name(name.value)
//        val ctorName = Ctor.Name(name.value)
//        val shellClassName = Type.Name(name.value + "Shell")
//        val shellCtorName = Ctor.Name(name.value + "Shell")
//        val shellObjectName = Term.Name(name.value + "Shell")
//        val updatedCls =
//          q"""
//             final class $className[T, Param] (val value : $TFace) extends AnyVal with
//               _root_.singleton.twoface.impl.Checked1Param[$className, $Cond, $Msg, $TFace, $ParamFace, T] with
//               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString())}[T] {
//               @inline def getValue : $TFace = value
//             }
//           """
//        val companion =
//          q"""
//             object $objectName extends _root_.singleton.twoface.impl.Checked1Param.Builder[$className, $Cond, $Msg, $TFace, $ParamFace] {
//               type Shell[T, Param] = $shellClassName[T, Param]
//             }
//           """
//        val shellCls =
//          q"""
//             final class $shellClassName[T, Param] extends
//               _root_.singleton.twoface.impl.Checked1ParamShell[$className, $TFace, T, Param] {
//               def apply(value : $TFace) : $className[T, Param] = new $ctorName[T, Param](value)
//             }
//           """
//        val shellCompanion =
//          q"""
//             object $shellObjectName extends _root_.singleton.twoface.impl.Checked1ParamShell.Builder[$shellClassName, $className, $Cond, $Msg, $TFace, $ParamFace] {
//               def create[T, Param] : $shellClassName[T, Param] = new $shellCtorName[T, Param]
//             }
//           """
//        Term.Block(Seq(updatedCls, companion, shellCls, shellCompanion))
//      case _ =>
////        println(defn.structure)
//        abort("@checked must annotate a class.")
//    }
//  }
//}
//
//
