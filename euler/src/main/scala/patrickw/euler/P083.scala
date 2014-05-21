package patrickw.euler

import patrickw.util.{Time, LatticeDirection}
import collection.immutable.Queue
import annotation.tailrec

object P083 {
  protected type Position = (Int, Int)

  protected val inputFile = "input/matrix.txt"
  protected val inputSize = 80
  protected val origin = (0, 0)
  protected val destination = (inputSize - 1, inputSize - 1)
  protected val allDirections = LatticeDirection.values

  protected val input = io.Source.fromInputStream(getClass.getResourceAsStream(inputFile)).getLines()
    .map(_.split(",").map(_.toInt))
    .toArray


  def main(args: Array[String]) {
    println(mspDijkstra(origin, Some(destination), allDirections).get(destination))
  }

  protected def isValidPosition(position: Position): Boolean =
    0 <= position._1 && position._1 < inputSize && 0 <= position._2 && position._2 < inputSize

  protected def costAtPosition(position: Position): Int =
    input(position._2)(position._1)

  protected def msp(origin: Position, destination: Option[Position], moveDirections: Set[LatticeDirection]): Map[Position, Int] = {
    //
    // BFS based algorithm with revisitation when the cost for a node is lowered
    //
    @tailrec
    def f(queue: Queue[Position], minCosts: Map[Position, Int]): Map[Position, Int] = {
      if (queue.isEmpty) {
        minCosts
      } else {
        val (position, queueTail) = queue.dequeue
        val cost = minCosts(position)
        val moves = moveDirections
          .map(_.apply(position))
          .filter(p => isValidPosition(p))
          .map(p => (p, cost + costAtPosition(p)))
          .filterNot { case (p, costP) => minCosts.get(p).exists(_ <= costP) }
        val newQueue = queueTail ++ moves
          .filterNot { case (p, costP) => destination.exists(_ == p) || queue.contains(p) }
          .map(_._1)
        val newMinCosts = minCosts ++ moves

        f(newQueue, newMinCosts)
      }
    }

    f(Queue(origin), Map(origin -> costAtPosition(origin)))
  }

  protected def mspDijkstra(origin: Position, destination: Option[Position], moveDirections: Set[LatticeDirection]): Map[Position, (Int, Boolean)] = {
    //
    // Dijkstra's Algorithm, visiting the node with the lowest tentative cost means we only need to hit each node once
    //
    @tailrec
    def f(toVisit: Set[Position], minCosts: Map[Position, (Int, Boolean)]): Map[Position, (Int, Boolean)] = {
      if (toVisit.isEmpty) {
        minCosts
      } else {
        val (position, cost) = toVisit.foldLeft((toVisit.head, minCosts(toVisit.head)._1)) {
          case ((minP, minCost), nextP) => {
            val nextCost = minCosts(nextP)._1
            if (nextCost < minCost) (nextP, nextCost) else (minP, minCost)
          }
        }

        if (destination.exists(_ == position)) {
          minCosts + (position -> (cost, true))
        } else {
          val moves = moveDirections
            .map(_.apply(position))
            .filter(p => isValidPosition(p) && !minCosts.get(p).exists(_._2))
          val newToVisit = toVisit - position ++ moves
          val newMinCosts = minCosts + (position -> (cost, true)) ++ moves
            .map(p => p -> (cost + costAtPosition(p), false))
            .filterNot { case (p, (tentativeCost, _)) => minCosts.get(p).exists(_._1 < tentativeCost) }

          f(newToVisit, newMinCosts)
        }
      }
    }

    f(Set(origin), Map(origin -> (costAtPosition(origin), false)))
  }
}
