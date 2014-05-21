package patrickw.util

object Factorial {
  def apply[A](n: A)(implicit num: Integral[A]): A = {
    import num._
    def f(n: A, acc: A = one): A =
      if (n > zero)
        f(n - one, n * acc)
      else
        acc

    if (n >= zero && (n % one equiv zero))
      f(n)
    else
      throw new IllegalArgumentException("n must be a non-negative integer")
  }

  def unapply[A](x: A)(implicit num: Integral[A]): Option[A] = {
    import num._
    def f(x: A, n: A = one): Option[A] =
      if (x equiv n)
        Some(n)
      else if (x % n equiv zero)
        f(x / n, n + one)
      else
        None

    if (x >= one)
      f(x)
    else
      None
  }
}
