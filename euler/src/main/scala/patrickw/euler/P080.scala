package patrickw.euler

import patrickw.util._

object P080 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 100 else args(0).toInt))

  def f(to: Int): BigInt = {
    val b = BigInt(10).pow(to - 1).pow(2)
    (1 to to view)
      .filterNot(Sqrt.isSquare)
      .map(n => Sqrt.floored(b * n))
      .map(n => DigitList(n).view.take(to).sum)
      .sum
  }
}
