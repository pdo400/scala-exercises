package patrickw.util

class Rational[A] private(n: A, d: A)(implicit num: Integral[A]) extends Tuple2[A, A](n, d) {
  def numerator: A = _1
  def denominator: A = _2
}

object Rational {
  def apply[A](numerator: A, denominator: A)(implicit num: Integral[A]): Rational[A] = {
    import num._

    val gcd = Gcd(numerator, denominator)
    signum(denominator) match {
      case 0 => error("d must be non-zero")
      case 1 => new Rational(numerator / gcd, denominator / gcd)
      case -1 => new Rational(negate(numerator / gcd), negate(denominator / gcd))
    }
  }

  def apply[A](integer: A)(implicit num: Integral[A]): Rational[A] =
    apply(integer, num.one)

  def unapply[A](q: Rational[A]): Option[(A, A)] =
    Some(q.asInstanceOf[(A, A)])

  class FractionalRational[A](implicit num: Integral[A]) extends Fractional[Rational[A]] {
    //
    // To prevent overflow we reduce intermediate values rather than relying on the constructor
    //

    import math.Integral.Implicits._

    def plus(x: Rational[A], y: Rational[A]): Rational[A] = {
      val d = Lcm(x.denominator, y.denominator)
      Rational(x.numerator * (d / x.denominator) +  y.numerator * (d / y.denominator), d)
    }
    def minus(x: Rational[A], y: Rational[A]): Rational[A] = {
      val d = Lcm(x.denominator, y.denominator)
      Rational(x.numerator * (d / x.denominator) -  y.numerator * (d / y.denominator), d)
    }
    def times(x: Rational[A], y: Rational[A]): Rational[A] = {
      val a = Gcd(x.numerator, y.denominator)
      val b = Gcd(y.numerator, x.denominator)
      Rational((x.numerator / a) * (y.numerator / b), (x.denominator / b) * (y.denominator / a))
    }
    def div(x: Rational[A], y: Rational[A]): Rational[A] = {
      val a = Gcd(x.numerator, y.numerator)
      val b = Gcd(x.denominator, y.denominator)
      Rational((x.numerator / a) * (y.denominator / b), (x.denominator / b) * (y.numerator / a))
    }
    def compare(x: Rational[A], y: Rational[A]): Int = num.signum(minus(x, y).numerator)
    def negate(x: Rational[A]): Rational[A] = Rational(num.negate(x.numerator), x.denominator)
    def toDouble(x: Rational[A]): Double = x.numerator.toDouble / x.denominator.toDouble
    def toFloat(x: Rational[A]): Float = x.numerator.toFloat / x.denominator.toFloat
    def toLong(x: Rational[A]): Long = (x.numerator / x.denominator) toLong
    def toInt(x: Rational[A]): Int = (x.numerator / x.denominator) toInt
    def fromInt(x: Int): Rational[A] = Rational(num.fromInt(x))
  }

  implicit object RationalIntIsFractional extends FractionalRational[Int]
  implicit object RationalLongIsFractional extends FractionalRational[Long]
  implicit object RationalBigIntIsFractional extends FractionalRational[BigInt]
}
