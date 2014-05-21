package patrickw.util

class GeneralizedPolygonal[A](s: A)(implicit num: Integral[A]) extends Quadratic[A](num.minus(s, num.fromInt(2)), num.minus(num.fromInt(4), s), num.zero, num.fromInt(2))

object GeneralizedPolygonal {
  def apply[A: Integral](s: A) =
    new GeneralizedPolygonal(s)
}
