package patrickw.euler

import patrickw.util.{Integers, DigitList}

object P052 {
  def main(args: Array[String]) {
    println(f(if (args.isEmpty) 6 else args(0).toInt))
  }

  def f(count: Int): Int = {
    val range = 3 to count
    Integers.natural[Int]
      .map(n => ("1" + n).toInt)
      .filter(n => {
      val digits = DigitList(2 * n).sorted
      range.forall(i => DigitList(i * n).sorted == digits)
    }).next()
  }
}
