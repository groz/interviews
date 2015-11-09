/**
  * Given string of keystrokes for Nokia-type keypad find out all possible words.
  */

object PhoneKeypad extends App {

  // solution
  val keymap = Map(
    2 -> "abc",
    3 -> "def",
    4 -> "ghi",
    5 -> "jkl",
    6 -> "mno",
    7 -> "pqrs",
    8 -> "tuv",
    9 -> "wxyz"
  )

  def words(keys: List[Int]): Seq[String] =
    keys.foldLeft(Seq("")) { (acc, k) =>
      for {
        c <- keymap(k)
        s <- acc
      } yield s + c
    }

  // test
  val ws = words(List(2, 7, 9)) // add .toSet if uniqueness is required
  ws.foreach(println)
}
