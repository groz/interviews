/*
Find shortest path between given nodes in a graph represented by adjacency list.
*/

object ShortestPath extends App {

  type Graph[A] = Map[A, Set[A]]

  @annotation.tailrec
  def find[A](graph: Graph[A], src: List[List[A]], dst: A): List[A] =
    if (graph.isEmpty || src.isEmpty) Nil
    else {
      val paths = for {
        from :: rest <- src if graph.contains(from)
        to <- graph(from)
      } yield to :: from :: rest

      paths.find(_.head == dst) match {
        case Some(p) => p
        case _ => find(graph -- src.map(_.head), paths, dst)
      }
    }

  def path[A](graph: Graph[A], src: A, dst: A): List[A] = find(graph, List(List(src)), dst).reverse

  // test
  val graph = Map(
    1 -> Set(2, 5),
    2 -> Set(3),
    3 -> Set(4),
    4 -> Set(5),
    5 -> Set(4)
  )

  println( path(graph, 1, 4) )
}
