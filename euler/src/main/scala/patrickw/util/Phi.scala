package patrickw.util

object Phi {
  def apply[A](n: A)(implicit num: Integral[A]): A = {
    import num._
    (FactorMap(n) foldLeft one) { case (acc, (p, a)) => acc * (p - one) * Pow(p, a - 1) }
  }
}
