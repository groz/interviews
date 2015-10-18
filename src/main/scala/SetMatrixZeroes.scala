/*
Given a matrix substitute if an element is equal to zero overwrite the whole row and column with zeroes (in place).
*/

object SetMatrixZeroes extends App {

  def zero(arr: Array[Array[Int]]) = {
    val nRows = arr.size
    val nCols = arr.head.size

    // mark rows/cols for zeroing
    for {
      i <- 0 until nRows
      j <- 0 until nCols
      if arr(i)(j) == 0
    } {
      arr(i)(0) = 0
      arr(0)(j) = 0
    }

    // zero marked rows/cols
    for (i <- 0 until nRows) {
      if (arr(i)(0) == 0)
        for (j <- 1 until nCols) arr(i)(j) = 0
    }

    for (j <- 0 until nCols) {
      if (arr(0)(j) == 0)
        for (i <- 1 until nRows) arr(i)(j) = 0
    }
  }

  // test
  val input = Array(
    Array(1, 1, 1, 1),
    Array(1, 1, 1, 0),
    Array(1, 1, 0, 0),
    Array(1, 0, 0, 0)
  )

  zero(input)

  input.foreach { row =>
    println(row.mkString(" "))
  }
}
