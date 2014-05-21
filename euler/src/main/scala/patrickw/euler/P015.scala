package patrickw.euler

object P015 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 20 else args(0).toInt))

  def f(n: Int, x: Int = 0, y: Int = 0): Long =
    if (x == n || y == n)
      1L
    else
      f(n, x + 1, y) +
        f(n, x, y + 1)
}
