package patrickw.util

case class IntegralMod[A](n: A)(implicit num: Integral[A]) extends Integral[A] {
  import num._

  def lpr(x: A): A = ((x % n) + num.abs(n)) % n

  def compare(x: A, y: A): Int = num.compare(x % n, y % n)
  def times(x: A, y: A): A = (x * y) % n
  def minus(x: A, y: A): A = (x - y) % n
  def plus(x: A, y: A): A = (x + y) % n
  def quot(x: A, y: A): A = (x / y) % n
  def rem(x: A, y: A): A = (x % y) % n
  def negate(x: A): A = num.negate(x) % n
  def toDouble(x: A): Double = num.toDouble(x)
  def toFloat(x: A): Float = num.toFloat(x)
  def toLong(x: A): Long = num.toLong(x)
  def toInt(x: A): Int = num.toInt(x)
  def fromInt(x: Int): A = num.fromInt(x) % n
}
