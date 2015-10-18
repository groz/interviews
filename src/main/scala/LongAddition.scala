/*
You are given two linked lists representing two non-negative numbers.
The digits are stored in reverse order and each of their nodes contain a single digit.
Add the two numbers and return it as a linked list.
*/

object LongAddition extends App {

  // solution
  def add(as: List[Int], bs: List[Int], memory: Int): List[Int] =
    (as, bs) match {
      case (Nil, _) =>
        if (memory == 0) bs
        else add(List(memory), bs, 0)
      case (_, Nil) =>
        if (memory == 0) as
        else add(as, List(memory), 0)
      case (a :: atail, b :: btail) =>
        val sum = a + b + memory
        val (m, d) = (sum / 10, sum % 10)
        d :: add(atail, btail, m)
    }

  // test
  val result = add(List(1), List(9, 9, 9), 0)
  println(result.reverse)

}
