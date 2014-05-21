package patrickw.util

import annotation.tailrec

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 11/9/12
 * Time: 9:26 AM
 */
object Sudoku {
  def apply(start: Map[(Int, Int), Int]) = new Sudoku(start)

  private val size = 9
  private val boxSize = size / 3

  val positions = {
    for {
      row <- 0 until size
      col <- 0 until size
    } yield
      (row, col)
  }

  val values = 1 to size

  def linkedPositions(position: (Int, Int)) = toPositions(position)

  private val toRow = {
    for {
      row <- 0 until size
    } yield
      row -> (0 until size).map(col => (row, col))
  }.toMap

  private val toCol = {
    for {
      col <- 0 until size
    } yield
      col -> (0 until size).map(row => (row, col))
  }.toMap

  private val toBox = {
    for {
      row <- 0 until boxSize
      col <- 0 until boxSize
    } yield {
      for {
        i <- 0 until boxSize
        j <- 0 until boxSize
      } yield
        (boxSize * row + i, boxSize * col + j)
    }
  }.flatMap(box => box.map(position => position -> box)).toMap

  private val toPositions = {
    for {
      p <- positions
    } yield
      p -> (toRow(p._1) ++ toCol(p._2) ++ toBox(p)).toSet
  }.toMap
}

class Sudoku(board: Map[(Int, Int), Int]) {
  import Sudoku._

  def openPositions = Sudoku.positions filterNot board.contains

  def move(p: (Int, Int), v: Int) =
    if (board.contains(p) || (Sudoku.linkedPositions(p) exists (board.get(_).exists(_ == v))))
      None
    else
      Some(Sudoku(board + (p -> v)))

  def valueAt(r: Int, c: Int) = board((r, c))

  def valueAt(p: (Int, Int)) = board(p)

  def prettyPrint() {
    for (row <- 0 until Sudoku.size) {
      if (row > 0)
        println()

      for (col <- 0 until Sudoku.size) {
        if (col > 0)
          print(' ')

        print(board.get((row, col)).getOrElse('_'))
      }
    }
    println()
    println()
  }
}
