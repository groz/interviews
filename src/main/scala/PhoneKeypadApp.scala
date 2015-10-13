object PhoneKeypadApp extends App {

  /*
  Given numbers string on nokia keypad find out all possible words
  */

  // 12:50 PM
  val keymap = Map(
    2 -> "abc",
    3 -> "def",
    4 -> "ijk",
    5 -> "lmn",
    6 -> "opq",
    7 -> "rstu",
    8 -> "vw",
    9 -> "xyz"
  )

  def words(keys: List[Int]): Seq[String] =
    keys match {
      case Nil => Seq("")
      case k :: tail =>
        for {
          c <- keymap(k)
          str <- words(tail).map(c + _)
        } yield str
    }

  // 12:58 PM

  val ws = words(List(2, 2))
  println(ws)

}
