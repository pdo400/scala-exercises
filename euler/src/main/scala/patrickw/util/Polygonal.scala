package patrickw.util

class Polygonal[A](s: A)(implicit num: Integral[A]) extends GeneralizedPolygonal[A](s) {
  import num._

  override def apply(n: A): A =
    if (n < one) error("n must be >= 1") else super.apply(n)

  override def isDefinedAt(n: A): Boolean =
    if (n < one) false else true

  override def unapply(n: A): Set[A] =
    super.unapply(n) filter (_ >= one)
}

object Polygonal {
  def apply[A: Integral](s: A) =
    new Polygonal(s)
}
