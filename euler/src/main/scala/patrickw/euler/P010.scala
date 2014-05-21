package patrickw.euler

import patrickw.util.Primes.primes

object P010 {
  def main(args: Array[String]) =
    println(f(primes, if (args.isEmpty) 2000000 else args(0).toInt))

  def f(primes: Stream[Int], max: Int, acc: Long = 0): Long = {
    if (primes.head < max)
      f(primes.tail, max, acc + primes.head)
    else
      acc
  }
}
