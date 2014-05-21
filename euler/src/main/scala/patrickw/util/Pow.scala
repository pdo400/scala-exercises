package patrickw.util

object Pow {
  def apply[A](n: A, pow: Int)(implicit num: Integral[A]): A = {
    import num._

    def f(p: Int = pow, acc: List[Boolean] = Nil): List[Boolean] =
      p match {
        case 0 => acc
        case _ => f(p / 2, (p % 2 == 1) :: acc)
      }

    def g(l: List[Boolean], acc: A = one): A =
      l match {
        case Nil => acc
        case head :: tail => g(tail, if (head) acc * acc * n else acc * acc)
      }

    g(f())
  }
}

object LogFloor {
  def apply[A](n: A, base: Int)(implicit num: Integral[A]): Int = {
    import num._

    val b = fromInt(base)

    def f(v: A, p: Int): Int =
      if (v > n)
        p
      else
        f(v * b, p + 1)

    f(b, 0)
  }
}
