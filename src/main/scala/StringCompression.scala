/*
Write a method that performs string compression replacing consecutive characters.
Assume the string only contains lowercase characters a..z
Example: abbbcc
Result : ab3c2
*/

object StringCompression extends App {

  def compress(str: String) =
    str.foldRight(List.empty[(Char, Int)]) {
      case (c, (p, count) :: tail) if p == c => (p, count + 1) :: tail
      case (c, list) => (c, 1) :: list
    }.map {
      case (p, 1) => p.toString
      case (p, c) => p.toString + c
    }.mkString

  // test
  println(compress("abbaaaaaaaaaaaaaaabcc"))
  println(compress("aabc"))
}
