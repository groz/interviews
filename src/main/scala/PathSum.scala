/**
Given a binary tree and a sum, determine if the tree has a root-to-leaf path such that adding up all
the values along the path equals the given sum.
*/

object PathSum extends App {

  case class Tree(value: Int, left: Option[Tree], right: Option[Tree])

  def pathSum(node: Tree, sum: Int): Boolean =
    node match {
      case Tree(value, None, None) => sum == value

      case Tree(value, Some(left), None) => pathSum(left, sum - value)

      case Tree(value, None, Some(right)) => pathSum(right, sum - value)

      case Tree(value, Some(left), Some(right)) => pathSum(left, sum - value) || pathSum(right, sum - value)
    }

  // test

  val tree =
    Tree(5,
      Some(Tree(4,
        Some(Tree(11,
          Some(Tree(7, None, None)),
          Some(Tree(2, None, None))
        )),
        None
      )),
      Some(Tree(8,
        Some(Tree(13, None, None)),
        Some(Tree(4, None, Some(Tree(1, None, None))))
      ))
    )

  println(pathSum(tree, 22))
}
