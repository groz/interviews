/*
Implement different traversals of a tree
*/

object BinaryTreeTraversals extends App {

  case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

  def preorderFold[A, B](tree: Tree[A], b: B)(f: (B, A) => B): B =
    tree match {
      case Tree(a, None, None) =>
        f(b, a)

      case Tree(a, None, Some(right)) =>
        val current = f(b, a)
        preorderFold(right, current)(f)

      case Tree(a, Some(left), None) =>
        val current = f(b, a)
        preorderFold(left, current)(f)

      case Tree(a, Some(left), Some(right)) =>
        val current = f(b, a)
        val leftResult = preorderFold(left, current)(f)
        preorderFold(right, leftResult)(f)
    }

  def postorderFold[A, B](tree: Tree[A], b: B)(f: (B, A) => B): B =
    tree match {
      case Tree(a, None, None) =>
        f(b, a)

      case Tree(a, None, Some(right)) =>
        val rightResult = postorderFold(right, b)(f)
        f(rightResult, a)

      case Tree(a, Some(left), None) =>
        val leftResult = postorderFold(left, b)(f)
        f(leftResult, a)

      case Tree(a, Some(left), Some(right)) =>
        val leftResult = postorderFold(left, b)(f)
        val rightResult = postorderFold(right, leftResult)(f)
        f(rightResult, a)
    }

  def inorderFold[A, B](tree: Tree[A], b: B)(f: (B, A) => B): B =
    tree match {
      case Tree(a, None, None) =>
        f(b, a)

      case Tree(a, None, Some(right)) =>
        val current = f(b, a)
        inorderFold(right, current)(f)

      case Tree(a, Some(left), None) =>
        val leftResult = inorderFold(left, b)(f)
        f(leftResult, a)

      case Tree(a, Some(left), Some(right)) =>
        val leftResult = inorderFold(left, b)(f)
        val current = f(leftResult, a)
        inorderFold(right, current)(f)
    }

  // test

  val tree = Tree(
    ".",
    Some(Tree(
      "L",
      Some(Tree("LL", None, None)),
      Some(Tree("LR", None, None))
    )),
    Some(Tree(
      "R",
      Some(Tree("RL", None, None)),
      Some(Tree("RR", None, None))
    ))
  )

  val preorderResult = preorderFold(tree, "")(_ + " -> " + _)
  println(preorderResult)

  val postorderResult = postorderFold(tree, "")(_ + " -> " + _)
  println(postorderResult)

  val inorderResult = inorderFold(tree, "")(_ + " -> " + _)
  println(inorderResult)
}
