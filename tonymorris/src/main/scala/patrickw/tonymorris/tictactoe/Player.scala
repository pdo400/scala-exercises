package patrickw.tonymorris.tictactoe

sealed abstract class Player {
  def unary_! : Player
}

object Player {
  case object X extends Player { override def unary_! = O }
  case object O extends Player { override def unary_! = X }
  val values = Set(X, O)
}
