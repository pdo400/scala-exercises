package patrickw.euler

import patrickw.util.DivisorSigma


object P012 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 500 else args(0).toInt))

  def f(minDivisors: Int, n: Int = 1, divN: Int = DivisorSigma(1, 0), nEven: Boolean = false): Long = {
    val nPlus1 = n + 1
    val divNPlus1 = DivisorSigma(if (nEven) nPlus1 else nPlus1 / 2, 0)
    if (divN * divNPlus1 > minDivisors)
      1L * n * nPlus1 / 2
    else
      f(minDivisors, nPlus1, divNPlus1, !nEven)
  }
}
