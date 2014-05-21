package patrickw.util

object DigitList {
  def apply[A](n: A, radix: Int = 10)(implicit num: Integral[A]): List[Int] = {
    import num._

    val base = num.fromInt(radix)
    def f(n: A, acc: List[Int] = Nil): List[Int] =
      if (n > zero)
        f(n / base, (n % base).toInt :: acc)
      else
        acc

    if (n >= one && (n % one equiv zero))
      f(n)
    else
      throw new IllegalArgumentException("n must be a natural number")
  }

  def unapply[A](digits: Seq[Int], radix: Int = 10)(implicit num: Integral[A]): Option[A] = {
    import num._

    val base = num.fromInt(radix)
    Some(digits.foldLeft(zero)((acc, digit) => acc * base + fromInt(digit)))
  }
}
