package patrickw.euler

import patrickw.util.Primes
import patrickw.util.Primes.isPrime

object P050 {
  // This wastes information when reducing the length

  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1000000 else args(0).toInt))

  def f(below: Int): (Int, Int) = {
    def g(len: Int): Stream[(Int, Int)] =
      (primes(len, below) map (Pair(_, len))) #::: g(len - 1)

    g(maxLen(below)).head
  }

  def primes(len: Int, below: Int): Stream[Int] = {
    var terms = scala.collection.mutable.LinearSeq() ++ Primes.primes.take(len)
    var primes = Primes.primes.drop(len)
    var acc = terms.sum

    def next(): Stream[Int] = {
      acc -= terms.head
      terms = terms.tail
      acc += primes.head
      terms :+= primes.head
      primes = primes.tail

      if (acc >= below)
        Stream.Empty
      else if (isPrime(acc))
        acc #:: next()
      else
        next()
    }

    if (isPrime(acc))
      acc #:: next()
    else
      next()
  }

  def maxLen(below: Int, primes: Seq[Int] = Primes.primes, acc: Int = 0, count: Int = 0): Int =
    if (acc + primes.head >= below)
      count
    else
      maxLen(below, primes.tail, acc + primes.head, count + 1)

}
