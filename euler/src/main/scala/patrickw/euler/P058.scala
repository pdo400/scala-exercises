package patrickw.euler

import patrickw.util.{Primes, SpiralCorners}


object P058 {
  def main(args: Array[String]) = println(f(
    if (args.length < 2) 7 else args(1).toInt,
    if (args.length < 1) 0.1 else args(0).toDouble))

  def f(start: Int, ratio: Double): Int = {
    def g(pAcc: Int = 0, tAcc: Int = 0, s: Stream[(Int, List[Int])] = SpiralCorners[Int]()): Int = {
      val (length, corners) = s.head
      val p = pAcc + corners.count(Primes.isPrime(_))
      val t = tAcc + corners.length

      if (length > start && 1.0 * p / t < ratio) length else g(p, t, s.tail)
    }

    g()
  }
}
