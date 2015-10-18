/*
Given a collection of intervals, merge all overlapping intervals.

For example,
Given [1,3],[2,6],[8,10],[15,18],
return [1,6],[8,10],[15,18].

*/

object MergeIntervals extends App {

  type Interval = (Int, Int)

  def mergeIntervals(intervals: Seq[Interval]): Seq[Interval] = {

    val sortedIntervals = intervals.sortBy(_._1)

    sortedIntervals.foldLeft(List.empty[Interval]) { (acc, current) =>
      acc match {
        case Nil => current :: Nil
        case (pStart, pEnd) :: rest =>
          val (start, end) = current
          if (start < pEnd) (pStart, pEnd max end) :: rest
          else current :: acc
      }
    }
  }

  // test
  val result = mergeIntervals(Seq((1, 3), (2, 6), (8, 10), (15, 18)))
  println(result)
}
