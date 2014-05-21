package patrickw.euler

import patrickw.util.Sqrt

object P100 {
  val defaultFrom = BigInt(10).pow(12).+(1).toLong

  def main(args: Array[String]) =
    printUntil(if (args.isEmpty) defaultFrom else args(0).toLong)

  def printUntil(n: Long, solutions: Stream[(Long, Long)] = solutionsFrom()): Unit = {
    println(solutions.head)
    if (solutions.head._1 < n)
      printUntil(n, solutions.tail)
  }

  def solutionsFrom(n: Long = 4): Stream[(Long, Long)] = {
    def nextN(n: Long) =
      n % 4 match {
        case 0 => n + 1
        case m => n + (4 - m)
      }

    if (n % 1000000 == 0) println("At n = " + n)
    solutionFor(n) match {
      case Some(b) => (n, b) #:: solutionsFrom(nextN(if (n < 10000) 5 * n else 5.828 * n toLong))
      case None => solutionsFrom(nextN(n))
    }
  }

  def solutionFor(n: Long): Option[Long] = {
    val target = BigInt(n) * (n - 1) / 2
    val r = Sqrt.floored(target)

    if (r * (r + 1) == target)
      Some((r + 1).toLong)
    else
      None
  }
}
