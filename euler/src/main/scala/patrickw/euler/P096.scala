package patrickw.euler

import patrickw.util.Sudoku


object P096 {
  protected val inputFile = "input/sudoku.txt"
  protected val input = {
    for {
      group <- io.Source.fromInputStream(getClass.getResourceAsStream(inputFile)).getLines().grouped(10)
    } yield
      group.
        drop(1).
        flatten.
        zip(Sudoku.positions).
        flatMap { case (v, p) => if (v == '0') None else Some(p -> (v - '0')) }.
        toMap
  }.toSeq

  def main(args: Array[String]) {
    println((input map solve map (s => (0 until 3).foldLeft(0)((acc, c) => 10 * acc + s.valueAt(0, c)))).sum)
  }

  def solve(m: Map[(Int, Int), Int]): Sudoku = {
    def solve(s: Sudoku, o: List[(Int, Int)]): Option[Sudoku] =
      o match {
        case Nil => Some(s)
        case head :: tail => (Sudoku.values.view flatMap (v => s.move(head, v).flatMap(n => solve(n, tail)))).headOption
      }

    val start = Sudoku(m)

    solve(start, start.openPositions.toList).get
  }
}
