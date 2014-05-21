package patrickw.euler

import collection.mutable.HashMap
import patrickw.util.{Combinations, Primes, DigitList}

object P049 {
  def main(args: Array[String]) =
    f(if (args.isEmpty) 4 else args(0).toInt, if (args.size < 2) 3 else args(1).toInt) foreach (l => println(l.mkString(", ")))

  def f(digits: Int, length: Int): Iterable[Seq[Int]] = {
    Primes.fromFile
      .dropWhile(_ < BigInt(10).pow(digits - 1).intValue)
      .takeWhile(_ < BigInt(10).pow(digits).intValue)
      .foldLeft(HashMap[List[Int], List[Int]]())(
      (acc, p) => {
        val l = DigitList(p).sorted
        acc += (l -> (p :: acc.getOrElse(l, Nil)))
      })
      .values
      .filter(_.size >= length)
      .flatMap(
      l => {
        Combinations(l, length)
          .filter(c => (1 until length - 1) forall (i => c(i - 1) - c(i) == c(i) - c(i + 1)))
          .toStream
      })
  }
}
