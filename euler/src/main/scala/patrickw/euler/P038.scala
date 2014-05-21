package patrickw.euler

import patrickw.util.{Permutations, DigitList}

object P038 {

  def main(args: Array[String]) =
    println(from() head)

  def from(permutations: Stream[Seq[Int]] = Permutations(1 to 9 reverse, 4)): Stream[Int] = {
    val base = DigitList.unapply[Int](permutations.head).get
    val n = 100000 * base + 2 * base

    if (isPan(n))
      n #:: from(permutations.tail)
    else
      from(permutations.tail)
  }

  val pan = (for (i <- 1 to 9) yield math.pow(2, i)) sum

  def isPan(n: Int): Boolean =
    pan == DigitList(n).foldLeft(0.0)((acc, d) => acc + math.pow(2, d))
}
