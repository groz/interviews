/*
You are given two linked lists representing two non-negative numbers.
The digits are stored in reverse order and each of their nodes contain a single digit.
Implement addition, multiplication and subtraction operations for numbers in that form.
*/

object LongArithmetic extends App {

  def add(as: List[Int], bs: List[Int], memory: Int = 0): List[Int] =
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

  def mult(as: List[Int], d: Int, memory: Int = 0): List[Int] =
    as match {
      case Nil if memory == 0 => as
      case Nil => mult(List(memory), 1, 0)
      case a :: tail =>
        val sum = a * d + memory
        val (m, r) = (sum / 10, sum % 10)
        r :: mult(tail, d, m)
    }

  def mult(as: List[Int], bs: List[Int]): List[Int] = {
    val partials = for {
      (a, i) <- as.zipWithIndex
      shift = List.fill(i)(0)
    } yield shift ::: mult(bs, a)

    partials.reduce((a, b) => add(a, b))
  }

  def carry(ds: List[Int]): List[Int] =
    ds match {
      case Nil => Nil
      case a :: tail if a != 0 => (a - 1) :: tail
      case a :: tail => 9 :: carry(tail)
    }

  // assuming as > bs
  def subtract(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) => bs
      case (_, Nil) => as
      case (a :: atail, b :: btail) if a >= b =>
        (a - b) :: subtract(atail, btail)
      case (a :: atail, b :: btail) =>
        (a + 10 - b) :: subtract(carry(atail), btail)
    }

  // test
  val result = add(List(1), List(9, 9, 9))
  println(result.reverse)

  println( mult(List(3, 4), 3) )

  println( mult(List(3, 4), List(1, 2)) )

  println( subtract(List(2, 0, 1), List(1, 2)) )
}
