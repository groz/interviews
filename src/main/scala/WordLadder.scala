/*
Given two words (start and end) and a dictionary, find the shortest transformation sequence from start to end, such that:
- Only one letter can be changed at a time.
- Each intermediate word must exist in the dictionary.
*/

object WordLadder extends App {

  def dist(a: String, b: String) = a.zip(b).count(p => p._1 != p._2) + Math.abs(b.size - a.size)

  def ladder(dictionary: Set[String], start: String, end: String): Set[List[String]] =
    if (start == end) Set(List.empty)
    else for {
      next <- dictionary if dist(start, next) == 1
      rest <- ladder(dictionary - next, next, end)
    } yield next :: rest

  // test
  println(
    ladder(Set("hot", "dot", "dog", "lot", "log", "cog"), "hit", "cog").minBy(_.size)
  )

  println(
    ladder(Set("a", "b", "c"), "a", "c").minBy(_.size)
  )
}
