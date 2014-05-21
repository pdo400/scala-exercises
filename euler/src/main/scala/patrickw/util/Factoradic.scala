package patrickw.util

object Factoradic {
  def apply[A](n: A)(implicit num: Integral[A]): Seq[A] = {
    import num._
    def f(n: A, acc: List[A] = List(zero)): List[A] = {
      val size = fromInt(acc.size + 1)
      n match {
        case n if n equiv zero => acc
        case _ => f(n / size, n % size :: acc)
      }
    }

    f(n)
  }
}
