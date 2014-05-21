package patrickw.euler

import patrickw.util.FactorMap

object P047 {
  def main(args: Array[String]) =
    println(from(if (args.isEmpty) 4 else args(0).toInt).head)

  def from(factors: Int, n: Long = 1, s: Option[Long] = None): Stream[Long] =
    if (FactorMap(n).size >= factors)
      s match {
        case Some(s) =>
          if (n - factors + 1 >= s)
            (n - factors + 1) #:: from(factors, n + 1, Some(s))
          else
            from(factors, n + 1, Some(s))
        case None =>
          from(factors, n + 1, Some(n))
      }
    else
      from(factors, n + 1, None)
}
