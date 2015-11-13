/**
  * Implement prefix search trie for a given dictionary.
  */

object PrefixSearch extends App {

  case class LookupTree(node: Map[Char, LookupTree]) {

    def add(word: String, pos: Int = 0): LookupTree =
      if (pos == word.size) this
      else {
        val char = word(pos)
        val next = node.getOrElse(char, LookupTree.empty)
        LookupTree(node.updated(char, next.add(word, pos + 1)))
      }

    def contains(prefix: String, pos: Int = 0): Boolean =
      (pos == prefix.size) || node.get(prefix(pos)).fold(false) {
        _.contains(prefix, pos + 1)
      }

    def findNode(prefix: String, pos: Int = 0): Option[LookupTree] = {
      if (pos == prefix.size) Some(this)
      else node.get(prefix(pos)).fold(None: Option[LookupTree])(_.findNode(prefix, pos + 1))
    }

    def search(prefix: String): Set[String] = findNode(prefix).fold(Set.empty[String])(_.build(Set(prefix)))

    def build(from: Set[String]): Set[String] =
      if (node.isEmpty) from
      else for {
        s <- from
        (char, child) <- node
        w <- child.build(from.map(_ + char))
      } yield w
  }

  object LookupTree {
    val empty = LookupTree(Map.empty[Char, LookupTree])

    def apply(dictionary: Set[String]): LookupTree =
      dictionary.foldLeft(LookupTree.empty)((acc, w) => acc.add(w))
  }

  // test
  val lookup = LookupTree(Set("bye", "held", "hell", "world", "woo"))

  println(lookup.contains("hel"))
  println(lookup.contains("helloo"))
  println(lookup.contains("hh"))
  println(lookup.contains("wo"))

  println(lookup.search("h"))
}
