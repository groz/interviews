/**
  * Given number of nodes in a graph and sequence of edges
  * find the moment at which graph becomes connected.
  */

object DynamicConnectivity extends App {

  case class WeightedUnionFind(n: Int, id: Vector[Int], w: Vector[Int], components: Int) {

    def connect(i: Int, j: Int): WeightedUnionFind = {
      val (pi, pj) = (root(i), root(j))

      if (pi == pj) this
      else if (w(pi) < w(pj))
        WeightedUnionFind(n, id.updated(pi, pj), w.updated(pj, w(pi) + w(pj)), components - 1)
      else
        WeightedUnionFind(n, id.updated(pj, pi), w.updated(pi, w(pi) + w(pj)), components - 1)
    }

    def connected(i: Int, j: Int): Boolean = root(i) == root(j)

    @annotation.tailrec
    private def root(i: Int): Int =
      if (id(i) == i) i
      else root(id(i))
  }

  object WeightedUnionFind {
    def apply(n: Int): WeightedUnionFind = WeightedUnionFind(n, Vector.range(0, n), Vector.fill(n)(1), n)
  }

  // test
  val input = List(
    0 -> 1,
    1 -> 2,
    2 -> 1,
    3 -> 2,
    3 -> 1,
    3 -> 0,
    2 -> 3
  )

  @annotation.tailrec
  private def find(input: List[(Int, Int)], current: WeightedUnionFind, step: Int = -1): Int =
    (input, current.components) match {
      case (_, 1) => step
      case (Nil, _) => -1
      case ((i, j) :: tail, _) => find(tail, current.connect(i, j), step + 1)
    }

  println(find(input, WeightedUnionFind(4)))
}
