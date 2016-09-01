//package singleton.ops
//
//
//
//object MinusSpec {
//  //implicit val p = Minus.apply[Int,2,1]
////  implicit val p = new Minus[Int, 2, 1] {
////    type Out = 1
////    val value : Out = 1
////  }
//  val one : 1 = 1
//  val two : 2 = 2
//  def sub[A <: Int with Singleton, B <: Int with Singleton](a: A, b: B)(implicit p : Minus[Int, A, B]) : p.Out {} = p.value
//  val a = sub(2,1)
//  val b = sub(two,one)
//  val mt = Minus.materializeMinus[Int, 2, 1]
//
//}
//
//object Demo {
//  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
//  import scala.reflect._
//  import universe._
//  import scala.reflect.runtime.{currentMirror => m}
//  import scala.tools.reflect.ToolBox
//  import scala.reflect.macros.blackbox.Context
//  import scala.language.experimental.macros
//  val toolbox = m.mkToolBox()
//}
