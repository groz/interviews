/** ********************
Write a function to check that a binary tree is a valid binary search tree.
That means for any node n
  - All nodes to the left of n have values less than or equal to n
  - All nodes to the right of n have values greater than or equal to n

examples

Valid
(A)
    2
   / \
  2   2
 /     \
1       3


Not valid
(B)
  1
 / \
2   3

(C)
    3
   / \
  2   5
 / \
0   4

Function should take the root of the tree as input.
Should return a boolean: true if a valid BST, false if not valid.
  * *********************/

object ValidSearchTree extends App {

  case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) {

    def foldInorder[B](b: B)(f: (B, A) => B): B = {
      val l = left.fold(b) { _.foldInorder(b)(f) }
      val current = f(l, value)
      right.fold(current) { _.foldInorder(current)(f) }
    }
  }

  // note: folding could be altered to stop when we know the result is false

  def isValid(tree: Tree[Int]): Boolean =
    tree.foldInorder(true, Int.MinValue) { (acc, value) =>
      val (isValid, previousValue) = acc
      (isValid && previousValue <= value, value)
    }._1

  // test
  val exampleA = Tree(2,
    Some(Tree(2, Some(Tree(1, None, None)), None)),
    Some(Tree(2, None, Some(Tree(3, None, None))))
  )

  val exampleB = Tree(1,
    Some(Tree(2, None, None)),
    Some(Tree(3, None, None))
  )

  val exampleC = Tree(3,
    Some(Tree(2, Some(Tree(0, None, None)), Some(Tree(4, None, None)))),
    Some(Tree(5, None, None))
  )

  println(isValid(exampleA)) // true
  println(isValid(exampleB)) // false
  println(isValid(exampleC)) // false
}
