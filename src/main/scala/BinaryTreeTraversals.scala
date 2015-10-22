/*
Implement different traversals of a tree
*/

object BinaryTreeTraversals extends App {

  case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) {

    def foldInorder[B](b: B)(f: (B, A) => B): B = {
      val l = left.fold(b) { _.foldInorder(b)(f) }
      val current = f(l, value)
      right.fold(current) { _.foldInorder(current)(f) }
    }

    def foldPreorder[B](b: B)(f: (B, A) => B): B = {
      val current = f(b, value)
      val l = left.fold(current) { _.foldInorder(current)(f) }
      right.fold(l) { _.foldInorder(l)(f) }
    }

    def foldPostorder[B](b: B)(f: (B, A) => B): B = {
      val l = left.fold(b) { _.foldInorder(b)(f) }
      val r = right.fold(l) { _.foldInorder(l)(f) }
      f(r, value)
    }

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

  println(tree.foldPreorder("")(_ + " -> " + _))
  println(tree.foldPostorder("")(_ + " -> " + _))
  println(tree.foldInorder("")(_ + " -> " + _))

}
