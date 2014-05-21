package patrickw.euler

import patrickw.util.DivisorSigma

object P023 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 28123 else args(0).toInt))

  def f(max: Int): Int = {
    val abundant = new Array[Boolean](max + 1)
    for (i <- 1 to max)
      abundant(i) = DivisorSigma(i) > 2 * i

    def sum(n: Int, acc: Int = 0): Int =
      if (n > max)
        acc
      else if ((1 to n / 2) forall (i => !(abundant(i) && abundant(n - i))))
        sum(n + 1, acc + n)
      else
        sum(n + 1, acc)

    sum(1)
  }
}
