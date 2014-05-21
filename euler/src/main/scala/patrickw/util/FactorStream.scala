package patrickw.util

import Primes.primes

object FactorStream {

  def apply[A](n: A)(implicit num: Integral[A]): Stream[A] = {
    import num._
    def f(n: A, primes: Stream[A]): Stream[A] =
      if (n equiv one)
        Stream.Empty
      else if (n < primes.head * primes.head)
        n #:: f(one, primes)
      else if (n % primes.head equiv zero)
        primes.head #:: f(n / primes.head, primes)
      else
        f(n, primes.tail)

    f(n, primes map (num.fromInt(_)))
  }

  def unapply[A](factors: Seq[A])(implicit num: Integral[A]): Option[A] =
    Some(factors product num)
}
