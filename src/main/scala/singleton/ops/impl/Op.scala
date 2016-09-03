package singleton.ops.impl

trait Op {
  type Out
  val value: Out {}
}

trait Op2[T, A <: T with Singleton, B <: T with Singleton] {
  type Out <: T with Singleton
  val value: Out {}
}