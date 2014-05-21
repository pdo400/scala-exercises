package patrickw.tonymorris.tictactoe

abstract sealed class Row
object Row {
  case object A extends Row
  case object B extends Row
  case object C extends Row
  val values = List(A, B, C)
}

abstract sealed class Column
object Column {
  case object `1` extends Column
  case object `2` extends Column
  case object `3` extends Column
  val values = List(`1`, `2`, `3`)
}

case class Position(row: Row, column: Column)

object Position {
  val values = Row.values flatMap (row => Column.values map (col => Position(row, col))) toList

  val positionToCombinations: Map[Position, List[List[Position]]] = {
    import Row._
    import Column._

    val winningCombinations =
      List(Position(A, `1`), Position(B, `2`), Position(C, `3`)) ::
        List(Position(A, `3`), Position(B, `2`), Position(C, `1`)) ::
        (for (row <- Row.values) yield Column.values map (col => Position(row, col)) toList) :::
        (for (col <- Column.values) yield Row.values map (row => Position(row, col)) toList)

    Position.values map (v => v -> (winningCombinations filter (_.contains(v)))) toMap
  }
}
