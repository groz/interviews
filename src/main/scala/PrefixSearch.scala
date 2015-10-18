/*
Implement fast prefix search for a given dictionary.

warning: below implementation is based on mutable stuff and probably overcomplicated
*/

import scala.collection.mutable

object PrefixSearch extends App {

  // solution
  class LookupTree(val c: Option[Char], val children: mutable.Set[LookupTree]) {

    def contains(prefix: String, pos: Int = 0): Boolean =
      if (pos == prefix.size) true
      else {
        val next = children.find(_.c.exists(_ == prefix(pos)))
        next.fold(false)(_.contains(prefix, pos + 1))
      }

    def findLast(prefix: String, pos: Int = 0): Option[LookupTree] =
      if (pos == prefix.size) Some(this)
      else {
        val next = children.find(_.c.exists(_ == prefix(pos)))
        next.fold(None: Option[LookupTree])(_.findLast(prefix, pos + 1))
      }

    def search(prefix: String): Set[String] =
      findLast(prefix).fold(Set.empty[String])(_.all(Set(prefix)))

    def all(result: Set[String]): Set[String] = {
      if (children.isEmpty) result
      else for {
        w: String <- result
        rest <- children.flatMap(ch => ch.all(result.map(_ + ch.c.get)))
      } yield rest
    }

    override def toString = s"${c.getOrElse('.')} -> ${children.mkString("[", ",", "]")}"
  }

  def createLookup(dictionary: Set[String]): LookupTree = {

    val lookupTree = new LookupTree(None, mutable.Set.empty[LookupTree])

    for (word <- dictionary) {
      var currentLookup = lookupTree

      for (c <- word) {
        val node: Option[LookupTree] = currentLookup.children.find(_.c.exists(_ == c))

        currentLookup = node.fold {
          val newTree = new LookupTree(Some(c), mutable.Set.empty[LookupTree])
          currentLookup.children.add(newTree)
          newTree
        } { identity }
      }
    }

    lookupTree
  }

  // test
  val lookup = createLookup(Set("hello", "held", "world", "bye", "holding"))
  println(lookup)

  println(lookup.contains("hel"))
  println(lookup.contains("helloo"))
  println(lookup.contains("hh"))
  println(lookup.contains("wo"))

  println(lookup.search("h"))
}
