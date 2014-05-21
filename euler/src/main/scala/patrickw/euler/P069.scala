package patrickw.euler

import patrickw.util.Primes

object P069 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1000000 else args(0).toInt))

  def f(to: Int): Int =
    Primes.primes.scanLeft(1)(_ * _).takeWhile(_ < to).last
}
