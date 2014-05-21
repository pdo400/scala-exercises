package patrickw.util

object Integers {
  def from[A](start: A, by: A)(implicit num: Integral[A]): Iterator[A] = {
    import num._
    Iterator.iterate(start)(_ + by)
  }

  def from[A](start: A)(implicit num:Integral[A]): Iterator[A] =
    from(start, num.one)

  def natural[A](implicit num: Integral[A]): Iterator[A] =
    from(num.one, num.one)

  def all[A](implicit num: Integral[A]): Iterator[A] = {
    import num._
    val two = fromInt(2)

    natural(num) map ({
      case e if e % two equiv zero => e / two
      case o => (one - o) / two
    })
  }
}
