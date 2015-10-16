/*
Find longest common subsequence of two strings

input:
ABEFGHT
ACDFTK

output:
AFT
*/

object LongestCommonSubsequence extends App {

  // solution
  def lcs(as: List[Char], bs: List[Char], acc: List[Char]): List[Char] =
    (as, bs) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (a :: atail, b :: btail) if a == b => lcs(atail, btail, a :: acc)
      case (a :: atail, b :: btail) =>
        val al = lcs(as, btail, acc)
        val bl = lcs(atail, bs, acc)
        if (al.size > bl.size) al else bl
    }

  def lcs(a: String, b: String): String = lcs(a.toList, b.toList, List.empty).reverse.mkString

  // test
  println(lcs("ABEFGHT", "ACDFTK"))
}
