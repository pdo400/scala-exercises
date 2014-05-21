package patrickw.util

class Quadratic[A](a: A, b: A, c: A, divisor: A)(implicit num: Integral[A]) extends PartialFunction[A, A] {
  import num._

  private val twoA = fromInt(2) * a
  private val fourA = fromInt(4) * a
  private val negativeB = negate(b)
  private val bSquared = b * b
  private val plusOrMinus = Set(plus _, minus _)

  override def apply(n: A): A =
    (a * n * n + b * n + c) match {
      case x if x % divisor equiv zero => x / divisor
    }

  override def isDefinedAt(n: A): Boolean =
    (a * n * n + b * n + c) % divisor equiv zero

  def unapply(n: A): Set[A] = {
    var s = Set.empty[A]
    Sqrt.exact(bSquared - fourA * (c - divisor * n)) match {
      case Some(determinant) => plusOrMinus foreach (op =>
        op.apply(negativeB, determinant) match {
          case v if v % twoA equiv zero => s += v / twoA
          case _ => ()
        })
      case None => ()
    }
    s
  }

  def isSolution(n: A): Boolean =
    ! unapply(n).isEmpty
}

object Quadratic {
  def apply[A](a: A, b: A, c: A, divisor: A)(implicit num: Integral[A]) =
    new Quadratic(a, b, c, divisor)(num)
}
