//package singleton.twoface
//
//import scala.meta._
//import scala.collection.immutable.Seq
//
//class checked0Param[Cond[_], Msg[_], TFace] extends scala.annotation.StaticAnnotation {
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
//             final class $className[T] (val value : $TFace) extends AnyVal with
//               _root_.singleton.twoface.impl.Checked0Param[$className, $TFace, T] with
//               ${Ctor.Name("_root_.singleton.twoface.impl.TwoFaceAny." + tFaceName.toString())}[T] {
//               @inline def getValue : $TFace = value
//             }
//           """
//        val companion =
//          q"""
//             object $objectName extends _root_.singleton.twoface.impl.Checked0Param.Builder[$className, $Cond, $Msg, $TFace] {
//               type Shell[T] = $shellClassName[T]
//             }
//           """
//        val shellCls =
//          q"""
//             final class $shellClassName[T] extends
//               _root_.singleton.twoface.impl.Checked0ParamShell[$className, $TFace, T] {
//               def apply(value : $TFace) : $className[T] = new $ctorName[T](value)
//             }
//           """
//        val shellCompanion =
//          q"""
//             object $shellObjectName extends _root_.singleton.twoface.impl.Checked0ParamShell.Builder[$shellClassName, $className, $Cond, $Msg, $TFace] {
//               def create[T] : $shellClassName[T] = new $shellCtorName[T]
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
//               _root_.singleton.twoface.impl.Checked1Param[$className, $TFace, $ParamFace, T] with
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
