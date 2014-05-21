package patrickw.util

import patrickw.util.Rational.FractionalRational

object Combinations {
  def apply[A](n: A, k: A)(implicit num: Integral[A]): A = {
    import num._

    require(zero <= n && zero <= k, "n, k must be >= 0")

    val t = n - k
    val kp = if (t < k) t else k
    val rat = new FractionalRational[A]()

    def f(i: A = one, acc: Rational[A] = Rational(one)): A =
      if (i > kp)
        if (acc.denominator equiv one)
          acc.numerator
        else
          error("Combinatoric function did not result in an integer, likely due to overflow")
      else
        f(i + one, rat.times(acc, Rational[A](n - (kp - i), i)))

    if ((n equiv zero) || n < k)
      zero
    else
      f()
  }

  def apply[T](s: Seq[T], choose: Int): Iterator[List[T]] = {
    require(0 <= choose, "choose must be >= 0")

    if (choose > s.length)
      Iterator.empty
    else
      new Iterator[List[T]] {
        private var haveNext = true
        private val arr = (0 until choose) toArray

        override def hasNext =
          haveNext

        override def next = {
          val t = arr.foldRight(List.empty[T])((v, acc) => s(v) :: acc)
          increment(choose - 1)
          t
        }

        private def increment(idx: Int) {
          if (idx < 0)
            haveNext = false
          else if (arr(idx) < s.size - choose + idx) {
            arr(idx) += 1
            (idx + 1 until choose) foreach (i => arr(i) = arr(idx) + i - idx)
          } else
            increment(idx - 1)
        }
      }
  }

  def unique[A](from: Map[A, Int], choose: Int): Stream[Map[A, Int]] = {
    require(choose >= 0, "choose must be >= 0")

    def f(m: Map[A, Int], need: Int, rem: Int, acc: Map[A, Int], back: List[() => Stream[Map[A, Int]]]): Stream[Map[A, Int]] =
      need match {
        case none if none == 0 =>
          acc #:: back.head()
        case all if all == rem =>
          m.foldLeft(acc){ case (acc, (a, n)) => acc.updated(a, n + acc.getOrElse(a, 0)) } #:: back.head()
        case some =>
          val (a, c) = m.head
          val newHead = if(c > 1) Some((a, c - 1)) else None
          val newBack = if (rem - c >= need) (() => f(m.tail, need, rem - c, acc, back)) :: back else back
          f(m.tail ++ newHead, need - 1, rem - 1, acc.updated(m.head._1, 1 + acc.getOrElse(m.head._1, 0)), newBack)
      }

    val gtZero = from.filter{ case (a, c) => c > 0 }
    val total = gtZero.values.sum
    if (total < choose)
      Stream.empty
    else
      f(gtZero, choose, total, Map.empty, List(() => Stream.empty[Map[A, Int]]))
  }

//  def apply[T](seq: Seq[T], choose: Int, acc: List[T] = Nil): Seq[List[T]] =
//    choose match {
//      case none if none == 0 =>
//        seq.companion.apply(acc.reverse)
//
//      case all if all == seq.size =>
//        apply(seq.tail, choose - 1, seq.head :: acc)
//
//      case some if some < seq.size =>
//        apply(seq.tail, choose - 1, seq.head :: acc) ++
//          apply(seq.tail, choose, acc)
//
//      case _ => error("seq.size must be >= choose")
//    }
}
