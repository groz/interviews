/*
Find 2 elements in the array that sum to a given number
*/

object TwoSum extends App {

  @annotation.tailrec
  def twoSum(as: List[Int], sum: Int, cache: Map[Int, Int] = Map.empty): Option[(Int, Int)] =
    as match {
      case Nil => None
      case a :: tail =>
        cache.get(a) match {
          case Some(b) => Some(a, b)
          case _ => twoSum(tail, sum, cache.updated(sum - a, a))
        }
    }

  // test
  val seq = (1 to 10).toList
  println( twoSum(seq, 13) )
  println( twoSum(seq, 27) )
}
