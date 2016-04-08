/*
Problem:
Implement regular expression matching with support for ’.’ and ’*’.
The matching should cover the entire input string.

`.` Matches any single character
`*` Matches zero or more of the preceding element.

*/

object RegexMatching extends App {

  def isMatch(cs: List[Char], ps: List[Char]): Boolean = (cs, ps) match {
    case (Nil, Nil) => true
    case (_, Nil) => false
    case (Nil, '*' :: Nil) => true
    case (Nil, _) => false
    case (c :: chars, '.' :: '*' :: pattern) => isMatch(chars, pattern) || isMatch(chars, ps)
    case (c :: chars, p :: '*' :: pattern) =>
      (c == p && (isMatch(chars, ps) || isMatch(chars, pattern))) || isMatch(cs, pattern)
    case (c :: chars, '.' :: pattern) => isMatch(chars, pattern)
    case (c :: chars, p :: pattern) => c == p && isMatch(chars, pattern)
  }

  def isMatch(s: String, p: String): Boolean = isMatch(s.toList, p.toList)

  // tests
  println(isMatch("aa", "a"))
  println(isMatch("aa", "aa"))
  println(isMatch("aaa", "aa"))
  println(isMatch("aa", "a*"))
  println(isMatch("aa", ".*"))
  println(isMatch("ab", ".*"))
  println(isMatch("aab", "c*a*b*"))
}
