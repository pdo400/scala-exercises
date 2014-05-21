package patrickw.tonymorris.tictactoe

trait Game {
  protected def board: Game.Board
  protected def history: Game.History

  def playerAt(position: Position): Option[Player] = board.get(position)
}

object Game {
  protected type Board = Map[Position, Player]
  protected type History = List[NonCompleted]

  trait New extends Game with NonCompleted

  trait NonNew extends Game {
    def takeBackMove: NonCompleted = history.head
  }

  trait InProgress extends Game with NonNew with NonCompleted

  trait NonCompleted extends Game {
    def whoseTurn: Player

    def move(position: Position): Option[Game] = {
      def isWinner: Boolean =
        Position.positionToCombinations(position).exists(_.forall(pos => pos == position || board.get(pos).exists(_ == whoseTurn)))

      if (board.contains(position))
        None
      else Some({
        val newBoard = board + (position -> whoseTurn)
        val newHistory = this :: history

        if (isWinner)
          new CompletedImpl(newBoard, newHistory, Some(whoseTurn))
        else if (newBoard.size == Position.values.size)
          new CompletedImpl(newBoard, newHistory, None)
        else
          new InProgressImpl(newBoard, newHistory, !whoseTurn)
      })
    }
  }

  trait Completed extends Game with NonNew {
    def whoWon: Option[Player]
  }

  private class GameImpl(_board: Board, _history: History) extends Game {
    protected def board: Board = _board
    protected def history: History = _history
  }

  private class NewGameImpl(whoGoesFirst: Player) extends GameImpl(Map.empty[Position, Player], Nil) with New {
    def whoseTurn: Player = whoGoesFirst
  }

  private class InProgressImpl(board: Board, history: History, _whoseTurn: Player)
    extends GameImpl(board, history) with NonNew with NonCompleted {
    def whoseTurn: Player = _whoseTurn
  }

  private class CompletedImpl(board: Board,  history: History, _whoWon: Option[Player])
    extends GameImpl(board, history) with NonNew {
    def whoWon: Option[Player] = _whoWon
  }

  def apply(whoGoesFirst: Player = Player.X): New = new NewGameImpl(whoGoesFirst)
}
