package patrickw.util

object Gcd {

  def apply[A](ns: A*)(implicit num: Integral[A]): A = {
    import num._
    def f(a: A, b: A): A =
      if (a < b)
        f(b, a)
      else if (b equiv zero)
        a
      else
        f(b, a % b)

    ns.foldLeft(zero)((acc, v) => f(acc, abs(v)))
  }

  def apply(ns: BigInt*)(implicit num: Integral[BigInt]): BigInt = {
    def apply(a: BigInt, b: BigInt): BigInt =
      a.gcd(b)

    ns.foldLeft(num.zero)(_.gcd(_))
  }
}
