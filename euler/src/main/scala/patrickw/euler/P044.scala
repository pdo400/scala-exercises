package patrickw.euler

import patrickw.util.{Integers, Polygonal}

object P044 {
  def main(args: Array[String]) =
    println(f().next())

  def f(): Iterator[((Long, Long), (Long, Long), (Long, Long), (Long, Long))] = {
    val pentagonals = Polygonal[Long](5)

    for {
      n <- Integers.natural[Long] map (n => (n, pentagonals(n)))
      val pn = n._2
      k <- Integers.natural[Long] takeWhile (_ < n._1) map (k => (k, pentagonals(k)))
      val pk = k._2
      val (sum, diff) = (pn + pk, pn - pk)
      if (pentagonals.isSolution(sum))
      if (pentagonals.isSolution(diff))
    } yield {
      (n, k, (pentagonals.unapply(sum).head, sum), ((pentagonals.unapply(diff).head, diff)))
    }
  }
}
