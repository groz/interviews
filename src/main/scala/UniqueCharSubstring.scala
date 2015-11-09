/**
  * Given a string and number M find length of the longest substring of M unique chars.
  * Input: abbbcddd, M = 3
  * Output: 7 (length of bbbcddd)
  */

object UniqueCharSubstring extends App {

  /**
    * Accumulate result until unique chars <= M
    * Backtrack to second unique symbol of current result
    */

  def find(start: List[Char], pos: List[Char], length: Int, unique: Set[Char], m: Int): Option[Int] =
    pos match {
      case Nil if unique.size < m => None
      case Nil => Some(length)
      case char :: tail =>
        val nextUnique = unique + char
        if (nextUnique.size <= m) {
          val from = if (nextUnique.size == 1) tail else start // setup backtracking position
          find(from, tail, length + 1, nextUnique, m)
        } else { // backtrack
          find(start, start, 0, Set.empty, m).map(_ max length) orElse Some(length)
        }
    }

  def find(s: String, m: Int): Option[Int] = {
    val list = s.toList
    find(list, list, 0, Set.empty, m)
  }

  // test
  println( find("abbbcddd", 3) )
  println( find("aabbcccddddee", 3) )
  println( find("aabbcccddddee", 1) )
}
