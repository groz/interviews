/**
  * Rotate the tree 60 degrees clockwise.
  *
  * Input:
  *       1
  *     /  \
  *    2   3
  *   / \ / \
  *  4   6   7
  *
  * Output:
  *       4
  *     /  \
  *    6   2
  *   / \ / \
  *  7   3   1
  *
  */

object RotateTree extends App {

  case class Tree[A](v: A, left: Option[Tree[A]], right: Option[Tree[A]]) {

    def leftSide: List[Tree[A]] = this :: left.map(_.leftSide).getOrElse(Nil)
    def rightSide: List[Tree[A]] = this :: right.map(_.rightSide).getOrElse(Nil)

    def rotate: Tree[A] = {
      val leaves = rightSide.map(t => Tree(t.v, None, None))

      leftSide.tail.foldLeft(leaves) { (previousLevel, current) =>
        for {
          (children, c) <- previousLevel.sliding(2).toList.zip(current.rightSide)
        } yield Tree(c.v, Some(children.last), Some(children.head))
      }.head
    }
  }

  // test
  val t4 = Tree(4, None, None)
  val t6 = Tree(6, None, None)
  val t7 = Tree(7, None, None)
  val t2 = Tree(2, Some(t4), Some(t6))
  val t3 = Tree(3, Some(t6), Some(t7))
  val t =  Tree(1, Some(t2), Some(t3))

  println(t.rotate)
}
