package patrickw.util

abstract class ContinuedFraction[A](implicit num: Integral[A]) {
  import num._

  def isFinite: Boolean
  def period: Option[(Int, Int)]
  def denominators(): Iterator[A]

  def convergents() =  new Iterator[Rational[A]]() {
    private val it = denominators
    private var n2 = (zero, one)
    private var n1 = (one, zero)

    def hasNext() = it.hasNext
    def next() = {
      val a = it.next()
      val n = Rational(a * n1._1 + n2._1, a * n1._2 + n2._2)
      n2 = n1; n1 = n
      n
    }
  }
}

object ContinuedFraction {

  trait Finite[A] extends ContinuedFraction[A] { override def isFinite = true; override def period = None }
  trait Infinite[A] extends ContinuedFraction[A] { override def isFinite = false }
  trait Periodic[A] extends Infinite[A]

  def sqrt[A](n: A)(implicit num: Integral[A]) =
    Sqrt.floored(n) match {
      case r if Sqrt.isExact(r, n) =>
        new ContinuedFraction[A]() with Finite[A] {
          override def denominators = Iterator(r)
        }
      case a0 =>
        new ContinuedFraction[A]() with Periodic[A] {
          import num._

          override def period = Some((1, denominators.indexWhere(_ equiv a0 * fromInt(2))))

          override def denominators = {
            var m = zero
            var d = one
            var a = a0

            def next(): A = {
              val t = a
              m = d * a - m
              d = (n - m * m) / d
              a = (a0 + m) / d
              t
            }

            Iterator.continually(next())
          }
        }
    }

  def e[A](implicit num: Integral[A]) =
    new ContinuedFraction[A]() with Infinite[A] {
      override def period = None
      override def denominators = {
        import num._

        val two = fromInt(2)
        var k = zero
        var l = List(two)

        def next(): A =
          l match {
            case head :: tail =>
              l = l.tail
              head
            case Nil =>
              k = k + one
              l = List(one, two * k, one)
              next()
          }

        Iterator.continually(next())
      }
    }
}
