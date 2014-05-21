package patrickw.euler

import patrickw.util.DivisorSigma


object P021 {

  def main(args: Array[String]) = {
    println(f(if (args.isEmpty) 10000 else args(0).toInt))
  }

  def f(max: Int): Int = {
    val a = new Array[Long](max)
    for (i <- 1 until max)
      a(i) = DivisorSigma(i) - i

    def sum(i: Int = 1, acc: Int = 0): Int =
      if (i == max)
        acc
      else if (a(i) != i && a(i) < max && a(a(i).toInt) == i)
        sum(i + 1, acc + i)
      else
        sum(i + 1, acc)

    sum()
  }
}
