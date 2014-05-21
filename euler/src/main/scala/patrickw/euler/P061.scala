package patrickw.euler

import patrickw.util._

object P061 {
  def main(args: Array[String]) = {
    val v = f()
    println(v mkString ", ")
    println(v sum)
  }

  def f(): List[Int] = {
    val polys = (3 to 8 reverse) map (i => Iterator.from(1) map Polygonal(i) dropWhile (_ < 1000) takeWhile (_ < 10000) map (DigitList(_, 100)) filter (_.forall(_ >= 10)) toSet)

    def g(seq: Seq[Set[List[Int]]], acc: List[List[Int]]): Option[List[List[Int]]] = {
      def canFollow(a: List[Int], b: List[Int]): Boolean =
        a.last == b.head

      if (seq.isEmpty)
        if (canFollow(acc.last, acc.head)) Some(acc) else None
      else {
        for {
          ss <- if (acc.isEmpty) Seq(seq) else Permutations(seq, 1, false)
          n <- if (acc.isEmpty) ss.head else ss.head filter (n => canFollow(n, acc.head)) filterNot (acc contains _)
          l <- g(ss.tail, n :: acc)
        } yield {
          return Some(l)
        }
        None
      }
    }

    g(polys, Nil).get map (l => DigitList.unapply[Int](l, 100).get)
  }
}
