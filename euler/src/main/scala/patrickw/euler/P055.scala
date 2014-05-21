package patrickw.euler

import scala._
import patrickw.util.combinator.{Memoize, Y}
import patrickw.util.DigitList

object P055 {
  def main(args: Array[String]) {
    println(f(if (args.isEmpty) 10000 else args(0).toInt))
  }

  def f(n: Int): Seq[Int] =
    1 until n filter (x => isLychrel(x))

  val isLychrel: BigInt => Boolean = {
    def outer(inner: (BigInt, List[Int], Int) => Boolean)(n: BigInt, reversed: List[Int], iterations: Int): Boolean =
      if (iterations >= 50)
        true
      else {
        val next = n + DigitList.unapply[BigInt](reversed).get
        val nextDigits = DigitList(next)
        val nextReversed = nextDigits.reverse
        if (nextDigits == nextReversed)
          false
        else
          inner(next, nextReversed, iterations + 1)
      }

    val memoized = Y(Memoize.firstArg[BigInt, List[Int], Int, Boolean] _ compose outer)

    x => memoized(x, DigitList(x).reverse, 0)
  }
}
