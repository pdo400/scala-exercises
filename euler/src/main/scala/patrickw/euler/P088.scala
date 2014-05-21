package patrickw.euler

import patrickw.util.FactorSums

object P088 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 12000 else args(0).toInt))

  def f(max: Int): Int = minimalProductSums.take(max).drop(1).toSet.sum

  def minimalProductSums: Stream[Int] = {
    val factorSums = FactorSums.memoized

    def from(k: Int, n: Int): Stream[Int] =
      if (factorSums(n).exists{ case (len, sums) => len <= k && sums.contains(n - (k - len)) })
        n #:: from(k + 1, k + 1)
      else
        from(k, n + 1)

    1 #:: from(2, 2)
  }
}
