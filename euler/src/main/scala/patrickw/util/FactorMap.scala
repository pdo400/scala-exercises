package patrickw.util

object FactorMap {
  type FactorMap[A] = Map[A, Int]

  def apply[A](n: A)(implicit num: Integral[A]): Map[A, Int] =
    apply(FactorStream(n))

  def apply[A](factors: Seq[A])(implicit num: Integral[A]): Map[A, Int] = {
    import num._
    def f(factors: Seq[A], acc: Map[A, Int] = Map()): Map[A, Int] =
      if (factors.isEmpty)
        acc
      else {
        val (take, drop) = factors.span(_ equiv factors.head)
        f(drop, acc + (factors.head -> take.size))
      }

    f(factors)
  }

  def unapply[A](m: Map[A, Int])(implicit num: Integral[A]): A = {
    import num._
    m.foldLeft(one)((acc, p) => acc * (for (i <- 1 to p._2) yield p._1).product)
  }
}
