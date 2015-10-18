/*
Given a sorted linked list, delete all duplicates such that each element appear only once.
*/

object SortedListRemoveDuplicates extends App {

  def dedup(as: List[Int]): List[Int] =
    as match {
      case Nil => Nil
      case a :: b :: tail if a == b => dedup(b :: tail)
      case a :: tail => a :: dedup(tail)
    }

  println(dedup(List(1, 1, 1, 2, 3, 3, 4, 5, 5, 5, 6)))
}
