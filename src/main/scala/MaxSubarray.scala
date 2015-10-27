/*
Find the largest sum of the contiguous subarray
*/

object MaxSubarray extends App {

  def maxSubarray(as: Seq[Int]): Int =
    as.foldLeft(0, 0) { (acc, a) =>
      val (current, total) = acc
      val next = 0 max (current + a)
      (next, total max next)
    }._2

  // test
  println( maxSubarray(Array(1, -2, 3, 5, -4, 5)) )
}
