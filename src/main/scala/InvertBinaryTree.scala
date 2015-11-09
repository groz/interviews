/**
  * Swap left and right of all the subtrees in a binary tree
  * aka "Homebrew problem"
  */

object InvertBinaryTree extends App {

  case class Tree[A](a: A, left: Option[Tree[A]], right: Option[Tree[A]]) {
    def invert: Tree[A] = Tree(a, right.map(_.invert), left.map(_.invert))
  }

  // test
  val t = Tree(4,
    Some(Tree(2,
      Some(Tree(1, None, None)),
      Some(Tree(3, None, None)))
    ),
    Some(Tree(7,
      Some(Tree(6, None, None)),
      Some(Tree(9, None, None)))
    )
  )

  println(t.invert)
}
