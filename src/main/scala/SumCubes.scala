/*
Find all a, b, c, d < N such as a^3 + b^3 == c^3 + d^3
*/

object SumCubes extends App {

  // solution
  def cubes(n: Int) = {
    val pairs = for {
      a <- 1 until n
      b <- a + 1 to n
    } yield (a, b)

    // note: mutable hashmap would've been faster

    pairs.foldLeft(Map.empty[Int, (Int, Int)], List.empty[(Int, Int, Int, Int)]) { (acc, pair) =>
      val (cache, result) = acc
      val (c, d) = pair
      val sum = c * c * c + d * d * d
      cache.get(sum) match {
        case None => (cache.updated(sum, (c, d)), result)
        case Some((a, b)) => (cache, (a, b, c, d) :: result)
      }
    }._2
  }

  // test
  println(cubes(1000))
}
