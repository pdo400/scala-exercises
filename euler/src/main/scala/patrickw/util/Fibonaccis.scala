package patrickw.util

object Fibonaccis {
  def apply[A](num: Numeric[A] = Numeric.IntIsIntegral): Stream[A] = {
    import num._
    def fibsFrom(a: A, b: A): Stream[A] =
      a #:: fibsFrom(b, a + b)

    fibsFrom(one, one)
  }
}
