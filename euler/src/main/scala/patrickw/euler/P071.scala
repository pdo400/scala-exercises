package patrickw.euler

import patrickw.util.{Integers, Gcd}


object P071 {
  def main(args: Array[String]) {
    println(f(
      if (args.length < 1) 3 else args(0).toLong,
      if (args.length < 2) 7 else args(0).toLong,
      if (args.length < 3) 1000000 else args(0).toLong))
  }

  def f[A](numerator: A, denominator: A, maxDenominator: A)(implicit num: Integral[A]): (A, A) = {
    import num._

    Integers.from(maxDenominator - denominator).takeWhile(_ <= maxDenominator)
      .map(x => (x * numerator / denominator - (if (x % denominator equiv zero) one else zero), x))
      .filter { case (n, d) => Gcd(n, d) == 1 }
      .reduceLeft((acc, next) => if (acc._1 * next._2 > next._1 * acc._2) acc else next)
  }
}
