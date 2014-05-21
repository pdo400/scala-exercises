package patrickw.util

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 5/28/12
 * Time: 7:20 AM
 */

object SpiralCorners {
  def apply[A]()(implicit num: Integral[A]): Stream[(A, List[A])] = apply(num.one)

  def apply[A](fromLength: A)(implicit num: Integral[A]): Stream[(A, List[A])] = {
    import num._

    val two = fromInt(2)
    val three = fromInt(3)

    require(fromLength % two equiv one, "fromLength must be equivalent to 1 mod 2")

    def from(n: A): Stream[(A, List[A])] =
      n match {
        case n if n equiv one => (one, List(one)) #:: from(three)
        case n =>
          val nsq = n * n
          val nm1 = n - one
          (n, List(nsq - three * nm1, nsq - two * nm1, nsq - nm1, nsq)) #:: from(n + two)
    }

    from(fromLength)
  }
}
