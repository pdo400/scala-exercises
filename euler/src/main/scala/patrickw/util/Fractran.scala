package patrickw.util

object Fractran {
  type Operation = (Map[Int, Int], Map[Int, Int])

  def apply(initialState: Int, operations: Traversable[(Int, Int)]): Stream[(Operation, Map[Int, Int])] = {
    val l = operations map (p => (FactorMap(p._1), FactorMap(p._2)))
    def statesFrom(s: Map[Int, Int]): Stream[(Operation, Map[Int, Int])] =
      nextState(s, l) match {
        case Some(t) => t #:: statesFrom(t._2)
        case None => Stream.empty
      }

    statesFrom(FactorMap(initialState))
  }

  def nextState(s: Map[Int, Int], l: Traversable[Operation]): Option[(Operation, Map[Int, Int])] =
    if (l.isEmpty)
      None
    else
      applyOp(s, l.head) match {
        case Some(s) => Some((l.head, s))
        case None => nextState(s, l.tail)
      }

  def applyOp(s: Map[Int, Int], op: Operation): Option[Map[Int, Int]] = {
    val (n, d) = op
    if (d exists (p => p._2 > s.getOrElse(p._1, 0) + n.getOrElse(p._1, 0)))
      None
    else
      Some({
        for {
          p <- s.keySet ++ n.keySet
          val e = s.getOrElse(p, 0) + n.getOrElse(p, 0) - d.getOrElse(p, 0)
          if e > 0
        }
        yield
          p -> e
      } toMap)
  }
}
