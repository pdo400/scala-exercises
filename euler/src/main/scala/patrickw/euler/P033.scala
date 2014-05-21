package patrickw.euler

import patrickw.util.{Gcd, Permutations}

object P033 {

  def main(args: Array[String]) = {
    println(f())
  }

  def f(): (Int, Int) = {
    (Permutations(1 to 9, 2) flatMap (d => {
      val (a, b) = (d(0), d(1))
      for (i <- 1 to (if (a > b) a - 1 else a)) yield
        (10 * i + a, 10 * a + b)
    }) filter (p => {
      val (a, b) = p
      a * (b % 10) == (a / 10) * b
    }) foldLeft ((1, 1)))((acc, v) => {
      val (n, d) = (acc._1 * v._1, acc._2 * v._2)
      val gcd = Gcd(n, d)
      (n / gcd, d / gcd)
    })
  }
}
