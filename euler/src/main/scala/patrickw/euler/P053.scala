package patrickw.euler

import scala._
import patrickw.util.{Integers, Combinations}


object P053 {
  def main(args: Array[String]) {
    println(f(
      BigInt(if (args.isEmpty) 100 else args(0).toInt),
      BigInt(if (args.length < 2) 1000000 else args(1).toInt)
    ))
  }

  def f[A](maxN: A, threshold: A)(implicit num: Integral[A]): A = {
    import num._

    val two = fromInt(2)

    def g(n: A): A = {
      val stop = (n / two) + one

      def h(r: A = zero): A =
        if (r == stop)
          zero
        else if (Combinations(n, r) > threshold)
          if((n % two) equiv zero) two * (stop - r) - one else two * (stop - r)
        else
          h(r + one)

      h()
    }

    def i(n: A = one, acc: A = zero): A =
      if (n > maxN)
        acc
      else
        i(n + one, acc + g(n))

    i()
  }

  def f2[A](maxN: A, threshold: A)(implicit num: Integral[A]): A = {
    import num._

    val two = fromInt(2)

    Integers.from(one).takeWhile(_ <= maxN).foldLeft(zero) {
      case (acc, n) =>
        val stop = (n / two) + one

        Integers.from(zero)
          .takeWhile(_ < stop)
          .map(r => (if (Combinations(n, r) > threshold) two * (stop - r) - (if ((n % two) equiv zero) one else zero) else zero))
          .find(_ > zero)
          .getOrElse(zero) + acc
    }
  }
}
