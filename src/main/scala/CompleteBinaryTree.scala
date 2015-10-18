/*
Check if the binary tree is complete.
In a complete binary tree every level, except possibly the last, is completely filled,
and all nodes in the last level are as far left as possible.
*/

object CompleteBinaryTree extends App {

  val branchFactor = 2

  case class Tree(children: Seq[Tree])

  def nextLevel(nodes: Seq[Tree]) = nodes.flatMap(_.children)

  def isComplete(nodes: Seq[Tree], level: Int): Boolean = {
    val next = nextLevel(nodes)

    if (next.isEmpty) {
      // last level, check that all full nodes are to the left
      val (full, notfull) = nodes.span(_.children.size == branchFactor)
      notfull.forall(_.children.size < branchFactor)
    } else {
      nodes.size == Math.pow(branchFactor, level) && isComplete(next, level + 1)
    }
  }

  def isComplete(root: Tree): Boolean = isComplete(Seq(root), 0)

  // test
  val leaf = Tree(Seq.empty)
  val complete = Tree(Seq(leaf, leaf))

  val noncomplete = Tree(Seq(
    Tree(Seq(Tree(Seq(leaf)), leaf)),
    leaf
  ))

  println(isComplete(leaf))
  println(isComplete(complete))
  println(isComplete(noncomplete))
}
