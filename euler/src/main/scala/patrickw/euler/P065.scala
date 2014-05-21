package patrickw.euler

import patrickw.util.{ContinuedFraction, DigitList}

object P065 {

  def main(args: Array[String]) = {
    println(f(if (args.isEmpty) 100 else args(0).toInt))
  }

  def f(convergent: Int): BigInt =
    DigitList(
      ContinuedFraction.e[BigInt]
        .convergents
        .drop(convergent - 1)
        .next()
        .numerator
    ).sum
}
