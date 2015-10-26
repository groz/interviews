/*
Pick random k numbers with equal probability from the sequence
using no more than O(k) extra space.

https://en.wikipedia.org/wiki/Reservoir_sampling
*/

object PickRandom extends App {

  import scala.util.Random

  def pickRandom[A](as: Seq[A], k: Int, rng: Random = new Random()): Seq[A] =

    as.view.zipWithIndex.foldLeft(Vector.empty[A]) { (reservoir, ai) =>
      val (a, i) = ai
      if (reservoir.size < k) reservoir :+ a
      else {
        val r = rng.nextInt(i + 1)
        if (r < k) reservoir.updated(r, a)
        else reservoir
      }
    }

  // test
  val frequencies =
    (1 to 10000).foldLeft(Map.empty[Int, Int]) { (m, _) =>
      val picked = pickRandom(0 to 10, 2)
      picked.foldLeft(m) { (m, e) => m.updated(e, m.getOrElse(e, 0) + 1) }
    }

  println(frequencies)
}
