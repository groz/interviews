/**
  * Given a set of allowed tokens find all possible tokenizations of a string.
  */

object TokenizeString extends App {

  import scala.collection.mutable

  def tokenize(tokens: Set[String], input: List[Char], word: String,
               cache: mutable.Map[(String, List[Char]), List[List[String]]]): List[List[String]] =
    cache.getOrElseUpdate((word, input),
      input match {
        case Nil =>
          if (tokens.contains(word)) List(List(word))
          else Nil
        case c :: tail =>
          val w = word + c
          if (tokens.contains(w)) {
            val short = tokenize(tokens, tail, "", cache).map(w :: _)
            val long = tokenize(tokens, tail, w, cache)
            short ++ long
          } else tokenize(tokens, tail, w, cache)
      })

  def tokenize(tokens: Set[String], input: String): List[List[String]] =
    tokenize(tokens, input.toList, "", mutable.Map.empty)

  // test
  tokenize(Set("a", "aa", "aaa", "abc", "bc"), "aaabc").foreach(println)
}
