package patrickw.euler

object P019 {
  type YearMonth = Pair[Int, Int]

  def YearMonth(year: Int, month: Int): YearMonth =
    Pair[Int, Int](year, month)

  val ordering = Ordering[YearMonth]

  def main(args: Array[String]) =
    println(if (args.isEmpty) f(YearMonth(1901, 1), YearMonth(2001, 1)) else null)

  def f(from: YearMonth, to: YearMonth, stream: Stream[Pair[YearMonth, Int]] = forwardFrom(), acc: Int = 0): Int = {
    if (ordering.gt(from, stream.head._1))
      f(from, to, stream.tail, acc)
    else if (ordering.gt(to, stream.head._1))
      f(from, to, stream.tail, if (stream.head._2 == 0) acc + 1 else acc)
    else
      acc
  }

  def daysInMonth(ym: YearMonth): Int = {
    val (year, month) = ym
    month match {
      case 1 => 31
      case 2 => if (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)) 29 else 28
      case 3 => 31
      case 4 => 30
      case 5 => 31
      case 6 => 30
      case 7 => 31
      case 8 => 31
      case 9 => 30
      case 10 => 31
      case 11 => 30
      case 12 => 31
    }
  }

  def incrementMonth(ym: YearMonth): YearMonth = {
    val (year, month) = ym
    if (month == 12)
      YearMonth(year + 1, 1)
    else
      YearMonth(year, month + 1)
  }

  def decrementMonth(ym: YearMonth): YearMonth = {
    val (year, month) = ym
    if (month == 1)
      YearMonth(year - 1, 12)
    else
      YearMonth(year, month - 1)
  }

  def forwardFrom(ym: YearMonth = YearMonth(1900, 1), dow: Int = 1): Stream[Pair[YearMonth, Int]] =
    Pair(ym, dow) #:: forwardFrom(incrementMonth(ym), (dow + daysInMonth(ym)) % 7)
}
