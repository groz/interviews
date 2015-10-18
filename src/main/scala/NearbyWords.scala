/*
Find all nearby words for a given one.
*/

object NearbyWords extends App {

  // placeholder implementations
  def nearbyChars(c: Char): Set[Char] = Set[Char](c, (c + 1).toChar, (c + 2).toChar)

  def isWord(w: String) = true

  // solution
  def nearby(input: String, maxDiff: Int): Set[String] =
    input.foldLeft(Set("")) { (acc, char) =>
      for {
        s <- acc
        c <- nearbyChars(char)
        next = s + c
        if next.zip(input).count(p => p._1 != p._2) <= maxDiff
      } yield next
    } filter isWord

  // test
  println(nearby("Hello", 1))
}
