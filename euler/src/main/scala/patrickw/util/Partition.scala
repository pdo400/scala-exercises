package patrickw.util

import combinator._

object Partition {
  // This version should only be used for one-offs, as it rebuilds the Memoization on every call
  def apply[A](n: Int)(implicit num: Integral[A]): A =
    Y(Memoize[Int, A] _ compose endomorphic[A])(n)

  def memoized[A](implicit num: Integral[A]): Int => A =
    Y(Memoize[Int, A] _ compose endomorphic[A])

  def endomorphic[A](implicit num: Integral[A]): ((Int => A) => (Int => A)) = {
    import num._

    val pentagonals = {
      val pfunk = GeneralizedPolygonal[Int](5);
      val (plus_, minus_) = (plus _, minus _)
      Integers.all[Int]
        .drop(1)
        .map(i => (if (i.abs % 2 == 0) minus_ else plus_, pfunk(i)))
        .toStream
    }

    f => n =>
      n match {
        case 0 => one
        case 1 => one
        case n if n < 0 => zero
        case n =>
        // Need to foldRight so smaller n are computed first and we don't blow the stack
          pentagonals
            .map({
            case (op, pent) => (op, n - pent)
          })
            .takeWhile({
            case (op, p) => p >= 0
          })
            .foldRight(zero)({
            case ((op, p), acc) => op(acc, f(p))
          })
      }
  }

  def slow[A: Integral](slow: (Int, Int) => A)(n: Int, m: Int): A = {
    val num = implicitly[Integral[A]]
    import num._

    n match {
      case 0 => one
      case 1 => one
      case _ => m match {
        case 1 => one
        case _ => {
          for {
            i <- 1 to (m min n)
          } yield
            slow(n - i, i min n - i)
        }.sum
      }
    }
  }
}
