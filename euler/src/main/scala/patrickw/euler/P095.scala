package patrickw.euler

import scala._
import patrickw.util.DivisorSigma


object P095 {
  def main(args: Array[String]) {
    println(f(if (args.isEmpty) 1000000 else args(1).toLong))
  }

  def f(maxElement: Long) = {
    val cf = cycleFinder(maxElement)
    ((for {
      n <- 1L until maxElement
      div = cf(n)
      if div > 0
    } yield (n, div)).foldLeft (0L, 0)) {
      case (acc, next) => if (next._2 > acc._2) next else acc
    }
  }

  def cycleFinder(maxElement: Long): Long => Int = {
    val map = scala.collection.mutable.Map.empty[Long, Int]

    def findLength(n: Long, acc: List[Long] = Nil): Int =
      (if(n <= maxElement) map.get(n) else Some(0)) match {
        case Some(length) => acc match {
          case Nil => length
          case _ => acc.foreach(x => map.update(x, 0)); 0
        }
        case None => acc.indexOf(n) + 1 match {
          case 0 =>
            findLength(DivisorSigma(n) - n, n :: acc)
          case length =>
            acc.zipWithIndex.foreach { case (n, i) => map.update(n, if (i < length) length else 0) }
            if (acc.length < length) length else 0
          }
      }

    n => findLength(n)
  }
}
