package patrickw.euler

import patrickw.util.Primes

object P037 {
  def main(args: Array[String]) =
    println(f() sum)

  def f(primes: Stream[Int] = Primes.primes.drop(4), acc: List[Int] = Nil): List[Int] =
    if (acc.size == 11)
      acc
    else if (isLeftTruncatable(primes.head) && isRightTruncatable(primes.head))
      f(primes.tail, primes.head :: acc)
    else
      f(primes.tail, acc)

  def isLeftTruncatable(p: Int): Boolean = {
    def f(p: Int, to: Int, current: Int = 10): Boolean =
      if (current > to)
        true
      else if (Primes.isPrime(p % current))
        f(p, to, current * 10)
      else
        false
    f(p, BigInt(10).pow(math.log10(p).floor.intValue).intValue)
  }

  def isRightTruncatable(p: Int): Boolean = {
    def f(p: Int, current: Int): Boolean =
      if (current == 1)
        true
      else if (Primes.isPrime(p / current))
        f(p, current / 10)
      else
        false
    f(p, BigInt(10).pow(math.log10(p).floor.intValue).intValue)
  }
}
