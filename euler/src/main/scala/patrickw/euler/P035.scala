package patrickw.euler

import patrickw.util.{Rotations, Primes, DigitList}

object P035 {
  def main(args: Array[String]) = {
    val l = f(if (args.isEmpty) 1000000 else args(0).toInt)
    println(l.mkString(", "))
    println(l size)
  }

  def f(under: Int): Traversable[Int] = {
    val primes = Primes.primes.takeWhile(_ < under).toSet
    for {
      p <- primes
      val pDigits = DigitList(p)
      if (Rotations(pDigits) forall (primes contains DigitList.unapply[Int](_).get))
    }
    yield
      p
  }
}
