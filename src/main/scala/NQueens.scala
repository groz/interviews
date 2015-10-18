/*
Arrange N chess queens on the N-by-N board so that no 2 queens threaten each other.
*/

object NQueens extends App {

  val n = 8

  type Cell = (Int, Int)

  val allCells = for {
    i <- 1 to n
    j <- 1 to n
  } yield (i, j)

  case class Board(rows: Set[Int], cols: Set[Int], diags1: Set[Int], diags2: Set[Int], cells: Set[Cell]) {

    def diag1(cell: Cell): Int = cell._1 - cell._2

    def diag2(cell: Cell): Int = (n - cell._1) - cell._2

    def add(cell: Cell): Board = Board(rows + cell._1, cols + cell._2, diags1 + diag1(cell), diags2 + diag2(cell), cells + cell)

    def canAdd(cell: Cell): Boolean =
      !cells.contains(cell) &&
      !rows.contains(cell._1) &&
      !cols.contains(cell._2) &&
      !diags1.contains(diag1(cell)) &&
      !diags2.contains(diag2(cell))
  }

  val emptyBoard = Board(Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)

  def queens: Set[Board] =
    (1 to n).foldLeft(Set(emptyBoard)) { (boards, i) =>
      boards.flatMap(nextBoards)
    }.filter(_.cells.size == n)

  def nextBoards(board: Board): Set[Board] = allCells.filter(board.canAdd).map(board.add).toSet

  // test
  val result = queens
  result.map(_.cells).foreach(println)
  println(result.size)
}
