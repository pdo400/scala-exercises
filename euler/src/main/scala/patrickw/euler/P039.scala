package patrickw.euler

import patrickw.util.Sqrt

object P039 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1000 else args(0).toInt))

  def f(n: Int): (Int, Int) = {
    val arr = new Array[Int](n + 1)
    for {
      a <- 1 to n / 3
      val aa = a * a
      b <- a to (n - a) / 2
      c <- Sqrt.exact(aa + b * b)
      if (a + b + c <= n)
    } {
      arr(a + b + c) += 1
    }
    val maxN = (1 to n) max Ordering[Int].on[Int](i => arr(i))
    (maxN, arr(maxN))
  }
}
