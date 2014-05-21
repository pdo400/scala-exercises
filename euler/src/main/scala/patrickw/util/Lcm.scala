package patrickw.util

object Lcm {
  def apply[A](ns: A*)(implicit num: Integral[A]): A = {
    import num._
    ns.foldLeft(num.one)((acc, v) => acc / Gcd(acc, v) * v)
  }
}
