/*
Given map of state transitions find the longest path.
No nodes have 2 paths out.
Paths can contain loops.

Example:
1 -> 2
2 -> 3
3 -> 1
4 -> 5

Output:
1 2 3
*/

object LongestPath extends App {

  def findPath(workflow: Map[Int, Int], src: Int): List[Int] =
    workflow.get(src).fold(List.empty[Int]) { dst =>
      dst :: findPath(workflow - src, dst)
    }

  def findLongest(workflow: Map[Int, Int]) = {
    val all = for {
      (src, _) <- workflow
      path = src :: findPath(workflow, src)
    } yield path

    all.maxBy(_.size)
  }

  val path = findLongest(Map(
    1 -> 2,
    2 -> 3,
    3 -> 1,
    10 -> 5,
    5 -> 1
  ))

  println(path)
}
