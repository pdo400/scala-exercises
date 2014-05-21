package patrickw.euler

import patrickw.util.ToWords

object P017 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1000 else args(0).toInt))

  def f(n: Int): Int = {
    val sb = new StringBuilder()
    for (i <- 1 to n)
      ToWords(i, sb)
    sb.length
  }
}
