package patrickw.euler

import scala._


object P099 {
  private val InputFile = "input/P099.txt"

  def main(args: Array[String]) = {
    println(f())
  }

  def f() = {
    val input = io.Source.fromInputStream(getClass.getResourceAsStream(InputFile)).getLines() .
      zipWithIndex .
      map { case (s, i) => val ss = s.split(','); (i + 1, ss(0).toDouble, ss(1).toDouble) }

    def base(v: (Int, Double, Double)) = v._2
    def exponent(v: (Int, Double, Double)) = v._3

    def g(lines: Iterator[(Int, Double, Double)], max: (Int, Double, Double) = (0, 0, 0)): (Int, Double, Double) = {
      if (lines.isEmpty)
        max
      else {
        import scala.math.log
        val next = lines.next()
        g(lines, if (exponent(max) * log(base(max)) > exponent(next) * log(base(next))) max else next)
      }
    }

    g(input)
  }
}
