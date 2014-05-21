package patrickw.euler

import patrickw.util.{DigitList, Factorial}

object P020 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 100 else args(0).toInt))

  def f(n: Int): Int =
    DigitList(Factorial(n)) sum
}
