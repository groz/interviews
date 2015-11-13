/**
  * Find the first occurrence of needle string in haystack sting
  */

object NeedleInHaystack extends App {

  // ugly and not properly tested
  // solution
  def find(needle: String, haystack: String, start: Int, nPos: Int, hPos: Int): Int =
    (nPos, hPos) match {
      case (nLen, _) if nLen == needle.length => start
      case (_, hLen) if hLen == haystack.length => -1

      case _ if start != -1 => // inside a match
        if (needle(nPos) == haystack(hPos))
          find(needle, haystack, start, nPos + 1, hPos + 1)
        else
          find(needle, haystack, -1, 0, hPos - nPos + 1)

      case _ => // outside a match
        if (needle(nPos) == haystack(hPos))
          find(needle, haystack, hPos, 1, hPos + 1)
        else
          find(needle, haystack, -1, 0, hPos + 1)
    }

  def find(needle: String, haystack: String): Int = find(needle, haystack, -1, 0, 0)

  // test
  println(find("abc", "aaabcabcdabc"))
}
