package patrickw.euler

import patrickw.util.ContinuedFraction

object P064 {

  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 10000 else args(0).toInt))

  def f(to: Int): Int =
    (1 to to)
      .view
      .flatMap(ContinuedFraction.sqrt(_).period)
      .filter(_._2 % 2 == 1)
      .size

}
