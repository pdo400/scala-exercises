package patrickw.euler

import patrickw.util.{Composites, Sqrt}
import patrickw.util.Primes.isPrime

object P046 {
  def main(args: Array[String]) =
    println(f().next())

  def f(): Iterator[Int] =
    Composites.from(1)
      .filter(n => n % 2 == 1)
      .filter(n => !((1 to Sqrt.floored((n - 2) / 2)) exists (r => isPrime(n - 2 * r * r))))
}
