package patrickw.euler

import patrickw.util.ContinuedFraction

object P057 {

  def main(args: Array[String]) = {
    println(f(if (args.isEmpty) 1000 else args(0).toInt))
  }

  def f(to: Int): Int =
    ContinuedFraction.sqrt(BigInt(2)).convergents
      .drop(1)
      .take(to)
      .filter(q => q.numerator.toString.length > q.denominator.toString.length)
      .size
}
