package patrickw.euler

import patrickw.util.{Pow, DigitList, Integers}

object P062 {

  def main(args: Array[String]) = {
    println(f[BigInt](if (args.isEmpty) 5 else args(0).toInt, if (args.size < 2) 3 else args(1).toInt))
  }

  def f[A](permutations: Int, power: Int)(implicit num: Integral[A]): A = {
    import num._
    val it = Integers.natural[A] map (n => Pow(n, power))

    def g(lim: A = one, acc: List[A] = Nil, n: Option[A] = None): A = {
      val next = n.getOrElse(it.next())
      if (next < lim)
        g(lim, next :: acc)
      else {
        acc
          .groupBy(n => DigitList(n).sorted)
          .filter(_._2.size == permutations)
          .map(_._2.last)
          .reduceLeftOption((x, y) => if (x < y) x else y)
        match {
          case Some(n) => n
          case None => g(lim * fromInt(10), Nil, Some(next))
        }
      }
    }

    g()
  }
}
