/*
Given number n output 2^n bit-sequences in such a way that consequent ones differ only by one bit.

Example input:
2

Example output:
00
01
11
10

Gray code:
https://en.wikipedia.org/wiki/Gray_code
*/

object GrayCode extends App {

  def grays(n: Int) =
    (1 until n).foldLeft(List(List(0), List(1))) { (previous, _) =>
      previous.map(0 :: _) ::: previous.reverse.map(1 :: _)
    }

  // test
  grays(3).map(_.mkString).foreach(println)
}
