/*
Given dictionary of words find if input string can be split into words from that dictionary.

Input:
["hello", "world"]
helloworld

Output:
true
*/

object WordBreak extends App {

  val dictionary = List("hello", "hell", "o", "world")

  def chain(input: String): List[List[String]] =
    if (input.isEmpty) List(List.empty[String])
    else for {
      prefix <- dictionary if input.startsWith(prefix)
      rest <- chain(input.stripPrefix(prefix))
    } yield prefix :: rest

  def canSplit(input: String): Boolean =
    chain(input).exists(c => c.map(_.size).sum == input.size)

  // test
  println(chain("helloworld"))
  println(canSplit("helloworld"))
}
