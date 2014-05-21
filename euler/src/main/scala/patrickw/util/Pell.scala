package patrickw.util

object Pell {
  def apply[A](d: A)(implicit num: Integral[A]): Iterator[Rational[A]] = {
    import num._

    val cf = ContinuedFraction.sqrt(d)
    cf.period match {
      case None => Iterator.empty
      case Some((from, length)) => {
        val n = length - 1 match {
          case r if r % 2 == 0 => 2 * r + 1
          case r => r
        }
        val Rational(x0, y0) = (cf.convergents drop n).next()
        var (xk, yk) = (one, zero)

        def next(): Rational[A] = {
          val next = Rational(x0 * xk + d * y0 * yk, x0 * yk + xk * y0)
          xk = next._1; yk = next._2
          next
        }
        Iterator.continually(next())
      }
    }
  }
}
