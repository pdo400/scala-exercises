package patrickw.util

import java.math.MathContext

class Sqrt[A](implicit num: Integral[A]) {
  import num._

  val exact = new PartialFunction[A, A]() {
    def isDefinedAt(n: A): Boolean =
      Sqrt.isSquare(n)

    def apply(n: A): A =
      Sqrt.exact(n).get
  }

  val floored = new PartialFunction[A, A]() {
    def isDefinedAt(n: A): Boolean =
      n >= zero

    def apply(n: A): A =
      Sqrt.floored(n)
  }
}

object Sqrt {
  case class BigDecimalPrecision(precision: BigDecimal, mathContext: MathContext)
  implicit val defaultPrecision = new BigDecimalPrecision(BigDecimal(0.0000001), MathContext.DECIMAL128)

  val maxExactLong = math.pow(2, 53).toLong
  val maxExactBigInt = BigInt(maxExactLong)

  def isSquare[A](n: A)(implicit num: Integral[A]): Boolean = {
    import num._
    n >= zero && isExact(floored(n), n)
  }

  def isExact[A](r: A, n: A)(implicit num: Integral[A]): Boolean = {
    import num._
    r * r equiv n
  }

  def exact[A](n: A)(implicit num: Integral[A]): Option[A] = {
    import num._
    if (n < zero)
      None
    else floored(n) match {
      case r if isExact(r, n) => Some(r)
      case _ => None
    }
  }

  def floored[A](n: A)(implicit num: Integral[A]): A = {
    import num._
      n match {
        case n if n < zero => error("n must be >= zero")
        case i: Int => fromInt(math.sqrt(i).toInt)
        case l: Long => if (l <= Sqrt.maxExactLong) fromInt(math.sqrt(l).toInt) else binarySearch(n)
        case bi: BigInt => if (bi <= Sqrt.maxExactBigInt) fromInt(math.sqrt(bi.doubleValue).toInt) else binarySearch(n)
        case _ => binarySearch(n)
      }
  }

  private def binarySearch[A](n: A)(implicit num: Integral[A]): A = {
    import num._

    val two = fromInt(2)

    def f(a: A, b: A): A = {
      if (a equiv b)
        a
      else {
        val mid = a + (b - a + one) / two
        mid * mid match {
          case x if x == n => mid
          case x if x < n => f(mid, b)
          case x if x > n => f(a, mid - one)
        }
      }
    }
    f(zero, (n + one) / two)
  }

  def approximate(n: BigDecimal)(implicit precision: BigDecimalPrecision): BigDecimal = {
    val n128 = BigDecimal(n.underlying, precision.mathContext)

    def initialEstimate(): BigDecimal =
      BigDecimal(floored(n.toBigInt))

    def improveEstimate(g: BigDecimal): BigDecimal =
      (g + n128 / g) / 2

    def f(g: BigDecimal): BigDecimal = {
      val next = improveEstimate(g)
      (next - g) abs match {
        case d if d < precision.precision => next
        case _ => f(next)
      }
    }

    if (n < 0)
      error("n must be >= zero")
    else
      f(initialEstimate())
  }
}

