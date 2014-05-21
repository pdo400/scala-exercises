package patrickw.util

object DivisorSigma {
  import FactorMap.FactorMap

  def apply[A](n: A, k: Int = 1)(implicit num: Integral[A]): A = apply(FactorMap(n), k)

  def apply[A](m: FactorMap[A], k: Int)(implicit num: Integral[A]): A = {
    require(k >= 0, "k must be non-negative")

    import num._
    k match {
      case 0 => m.map{ case (_, a) => fromInt(a + 1) }.product
      case k => m.map{ case (p, a) => (Pow(p, k * (a + 1)) - one) / (Pow(p, k) - one) }.product
    }
  }
}
