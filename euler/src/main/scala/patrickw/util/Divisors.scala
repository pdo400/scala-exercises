package patrickw.util

object Divisors {
  def apply[A](n: A)(implicit num: Integral[A]): Stream[A] = {
    val m = FactorMap(n)
    val to = m.values.sum

    Stream.from(0).takeWhile(_ <= to).map(Combinations.unique(m, _)).flatten.map(FactorMap.unapply(_))
  }

  def unapply[A](s: Set[A])(implicit num: Integral[A]): Option[A] =
    Some(s.max)
}
