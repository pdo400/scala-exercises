package patrickw.euler

import patrickw.util.SpiralCorners

object P028 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1001 else args(0).toInt))

  def f(len: Int): Int =
    SpiralCorners[Int]().takeWhile{ case (n, _) => n <= len }.foldLeft(0){ case (acc, (_, l)) => acc + l.sum }
}
