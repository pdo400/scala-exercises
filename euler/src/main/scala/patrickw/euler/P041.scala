package patrickw.euler

import patrickw.util.{DigitList, Primes, Permutations}

object P041 {
  def main(args: Array[String]) =
    println(f())

  def f(digits: Seq[Int] = 1 to 9 reverse): Int =
    if (digits.sum % 3 == 0)
      f(digits.tail)
    else
      Permutations(digits) map (DigitList.unapply[Int](_).get) filter (Primes.isPrime(_)) match {
        case head #:: tail => head
        case _ => f(digits.tail)
      }
}
