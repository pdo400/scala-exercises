package patrickw.euler

import patrickw.util.{Polygonal, LetterValues}
import patrickw.util.combinator.{Y, Memoize}

object P081 {
  private val InputFile = "input/matrix.txt"
  private val size = 80

  def main(args: Array[String]) = {
    val input = io.Source.fromInputStream(getClass.getResourceAsStream(InputFile)).getLines
      .map(_.split(",").map(_.toInt))
      .toArray

    println(f(input))
  }

  def f(input: Array[Array[Int]]): Int = {
    def msp(recur: (Int, Int) => Int): (Int, Int) => Int = (x, y) => {
      input(x)(y) + {
        if (x == 0 && y == 0)
          0
        else if (x == 0)
          recur(x, y - 1)
        else if (y == 0)
          recur(x - 1, y)
        else
          math.min(recur(x - 1, y), recur(x, y - 1))
      }
    }

    val memo = Y(Memoize[Int, Int, Int] _ compose msp)

    memo(size - 1, size - 1)
  }
}
