package patrickw.euler

import patrickw.util.{Permutations, DigitList}

object P032 {
  val digits = DigitList(123456789)

  def main(args: Array[String]) = {
    val v = {
      if (args.isEmpty)
        f(9, 9)
      else
        f(args(0).toInt, args(1).toInt)
    } sorted Ordering[Pair[Int, Int]].on[Triple[Int, Int, Int]](t => Pair(t._3, t._1))

    v foreach println
    println
    println(v.map(_._3).toSet.sum)
  }

  def f(n: Int, m: Int) = {
    for {
      i <- n to m
      j <- 1 to i / 2
      k <- j to i / 2
      if (i == 2 * (j + k) || i == 2 * (j + k) - 1)
    }
    yield
      g(digits take i, j, k)
  }.flatten

  def g(digits: List[Int], x: Int, y: Int): Stream[Triple[Int, Int, Int]] =
    for {
      aPerm <- Permutations(digits, x, false)
      val (aTake, aDrop) = aPerm.splitAt(x)
      val a = DigitList.unapply[Int](aTake).get
      bPerm <- Permutations(aDrop, y, false)
      val (bTake, bDrop) = bPerm.splitAt(y)
      val b = DigitList.unapply[Int](bTake).get
      val c = a * b
      val cList = DigitList(c)
      if (cList.size == bDrop.size)
      if (cList.size == cList.toSet.size)
      if (cList forall (bDrop contains _))
    }
    yield
      Triple(a, b, c)
}
