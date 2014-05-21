package patrickw.euler

import patrickw.util.Gcd

object P073 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 12000 else args(0).toInt))

  def f(to: Int): Int = {
    def g(n: Int): Int =
      ((n / 3) + 1 until (n + 1) / 2).foldLeft(0)((acc, x) => if (Gcd(n, x) == 1) acc + 1 else acc)

    (1 to to).foldLeft(0)(_ + g(_))
  }
}
