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
    src ::
      workflow
      .get(src)
      .map(dst => findPath(workflow - src, dst))
      .getOrElse(Nil)

  def findLongest(workflow: Map[Int, Int]) =
    workflow.keys.map(src => findPath(workflow, src)).maxBy(_.size)

  val path = findLongest(Map(
    1 -> 2,
    2 -> 3,
    3 -> 1,
    10 -> 5,
    5 -> 1
  ))

  println(path)
}
