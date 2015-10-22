/**
  Find maximum product of subsequence of length n in array.

  Example:
  Input: (1, 2, 3), n = 2
  Output: 6

  Input: (-9,-9, 2, 3), n = 2
  Output: 81
*/

object MaxProduct extends App {

  def product(as: Array[Int], n: Int, start: Int, end: Int, p: Int): Int =
    if (n > end - start + 1) 1
    else if (n == 0) p
    else {
      val left = product(as, n - 1, start + 1, end, p * as(start))
      val right = product(as, n - 1, start, end - 1, p * as(end))
      left max right
    }

  def maxProduct(as: Array[Int], n: Int): Int =
    product(as.sorted, n, 0, as.size - 1, 1)

  // test
  println(maxProduct(Array(-5, -4, -3, 1, 1, 2), 4))
}
