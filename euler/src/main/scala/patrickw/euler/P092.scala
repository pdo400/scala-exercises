package patrickw.euler

import patrickw.util._

object P092 {
  def main(args: Array[String]) = {
    //println(f(if (args.isEmpty) 10000000 else args(0).toInt))
    println(fPrime(if (args.isEmpty) 7 else args(0).toInt))
  }

  def f(until: Int): Int = {
    val t = new Array[Int](81 * math.log10(until).ceil.intValue)
    val s = (0 to 9) map (n => n * n) toArray

    def next(n: Int): Int =
      DigitList(n).foldLeft(0)((acc, v) => acc + s(v))

    def g(n: Int, acc: List[Int] = Nil): Int =
      n match {
        case 1 =>
          acc foreach (t(_) = 1)
          1
        case 89 =>
          acc foreach (t(_) = 89)
          89
        case _ =>
          if (t.size <= n)
            g(next(n), acc)
          else t(n) match {
            case 1 => g(1, acc)
            case 89 => g(89, acc)
            case _ => g(next(n), n :: acc)
          }
      }

    Integers.natural[Int]
      .takeWhile(_ < until)
      .filter(n => g(n) == 89)
      .size
  }

  def fPrime(digits: Int): BigInt = {
    val t = new Array[Int](81 * digits)
    val ds = 0 to 9
    val sq = ds map (n => n * n) toArray

    def buildCounts(digits: Int, acc: Map[Int, BigInt] = Map(0 -> 1)): Map[Int, BigInt] =
      if (digits == 0)
        acc
      else
        buildCounts(digits - 1, acc
          .toSeq
          .flatMap({ case (sum, count) => ds map (d => (sum + sq(d), count)) })
          .groupBy({ case (sum, count) => sum })
          .map({ case (sum, seq) => (sum, seq.foldLeft(BigInt(0))({ case (acc, (sum, count)) => acc + count })) }))

    def g(n: Int, acc: List[Int] = Nil): Int = {
      def next(n: Int): Int =
        DigitList(n).foldLeft(0)((acc, v) => acc + sq(v))

      n match {
        case 0 =>
          0
        case 1 =>
          acc foreach (t(_) = 1)
          1
        case 89 =>
          acc foreach (t(_) = 89)
          89
        case _ =>
          if (t.size <= n)
            g(next(n), acc)
          else t(n) match {
            case 1 => g(1, acc)
            case 89 => g(89, acc)
            case _ => g(next(n), n :: acc)
          }
      }
    }

    buildCounts(digits)
      .foldLeft(BigInt(0))({ case (acc, (sum, count)) => if (g(sum) == 89) acc + count else acc })
  }
}
