package patrickw.euler

import patrickw.util.DigitList

object P056 {

  def main(args: Array[String]) = {
    println(f(if (args.isEmpty) 100 else args(0).toInt))
  }

  def f(max: Int, i: BigInt = BigInt(1), j: Int = 1, acc: BigInt = BigInt(0)): BigInt =
    if (i == max)
      acc
    else if (j == max)
      f(max, i + 1, 1, acc)
    else
      f(max, i, j + 1, acc max DigitList(i pow j).sum)
}
