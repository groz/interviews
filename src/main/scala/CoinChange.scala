/*
Given list of coin denominations [1, 5, 10, 25, 100] find all possible ways to give change for a given sum.
*/

import scala.collection.mutable

object CoinChange extends App {

  def change(money: Int, coins: List[Int], cache: mutable.Map[(Int, List[Int]), Int] = mutable.Map.empty): Int =
    cache.getOrElseUpdate((money, coins), {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else change(money - coins.head, coins, cache) + change(money, coins.tail, cache)
    })

  // test
  val denominations = List(1, 5, 10, 25, 100)
  println(change(1000, denominations))
}
