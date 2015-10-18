/*
Check if a given string contains correct parentheses
*/

object ParenthesesCheck extends App {

  def check(cs: List[Char], n: Int): Boolean =
    if (n < 0) false
    else cs match {
      case Nil => n == 0
      case c :: tail =>
        if (c == '(') check(tail, n + 1)
        else if (c == ')') check(tail, n - 1)
        else check(tail, n)
    }

  def check(s: String): Boolean = check(s.toList, 0)

  // test
  println(check("(()")) // false
  println(check("()()")) // true
  println(check("()))")) // false
}
