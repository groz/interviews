/*
Given string of keystrokes for nokia keypad find out all possible words.
*/

object PhoneKeypadApp extends App {

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

  val ws = words(List(2, 3)) // add .toSet if needed
  println(ws)
}
