package patrickw.euler

import scalaz._
import Scalaz._
import patrickw.util.{Time, Integers, PythagoreanTriple}
import annotation.tailrec


object P075 {
  def main(args: Array[String]) {
    println(f(if (args.isEmpty) 1500000 else args(0).toInt))
  }

  def f(maxInclusive: Int): Int = {
    val l = PythagoreanTriple.primitivesWithGenerator().takeWhile {
      case ((m, _), _) => 2 * (m * m + m) <= maxInclusive
    }.map {
      case (_, p) => p._1 + p._2 + p._3
    }.toList.sorted

    @tailrec
    def g(ps: List[Int] = l, acc: Map[Int, Boolean] = Map.empty): Map[Int, Boolean] = {
      ps match {
        case Nil => acc
        case head :: tail => {
          if (head > maxInclusive)
            acc
          else {
            g(tail, acc ++ Integers.natural[Int].map(_ * head).takeWhile(_ <= maxInclusive).flatMap {
              case p => acc.get(p).cata(b => if (b) Some(p -> false) else None, Some(p -> true))
            })
          }
        }
      }
    }

    g().foldLeft(0) {
      case (acc, (_, b)) => if (b) acc + 1 else acc
    }
  }
}
