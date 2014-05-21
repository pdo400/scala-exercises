package patrickw.euler

import scala._
import patrickw.util.combinator.Memoize
import patrickw.util.{Factorial, DigitList}


object P074 {
  def main(args: Array[String]) {
    println(f(
      if (args.isEmpty) 60 else args(0).toInt,
      if (args.size < 2) 1000000 else args(1).toInt))
  }

  def f(cycleLength: Int, n: Int): Int = {
    val cf = cycleFinder
    (1 until n).map(cf).count(_ == cycleLength)
  }

  def cycleFinder(): Int => Int = {
    val map = scala.collection.mutable.Map.empty[Int, Int]
    val memoFac = Memoize(Factorial[Int] _)

    def findLength(n: Int, acc: List[Int] = Nil): Int =
      map.get(n).map(_ + acc.length).getOrElse(acc.indexOf(n) match {
        case -1 =>
          val next = DigitList(n).foldLeft(0)((acc, d) => acc + memoFac(d))
          findLength(next, n :: acc)
        case depth =>
          acc.zipWithIndex foreach {
            case (n, i) =>
              map.update(n, 1 + (if (depth > i) depth else i))
          }
          acc.length
      })

    n => findLength(n)
 }
}
