package patrickw.euler

object P006 {
  def main(args: Array[String]) =
    println(sumSquared(100) - sumSquares(100))

  def sumSquared(n: Int): Int = {
    n * n * (n + 1) * (n + 1) / 2 / 2
  }

  def sumSquares(n: Int): Int =
    (1 to n) map (x => x * x) sum
}
