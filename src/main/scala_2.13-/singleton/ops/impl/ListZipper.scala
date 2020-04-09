package singleton.ops.impl

object ListZipper {
  def apply[T](list1 : List[T], list2 : List[T]) = (list1, list2).zipped
}
