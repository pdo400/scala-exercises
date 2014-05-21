package patrickw.euler

import patrickw.util.Primes.primes

object P007 {
  def main(args: Array[String]) =
    println(primes((if (args.isEmpty) 10001 else args(0).toInt) - 1))
}
