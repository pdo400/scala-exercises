package patrickw.euler

object P009 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1000 else args(0).toInt))

  def f(sum: Int, a: Int = 1, b: Int = 2): Option[Int] = {
    val c = sum - a - b
    if (c <= 0)
      None
    else if (a * a + b * b == c * c)
      Some(a * b * c)
    else if (a + 1 < b)
      f(sum, a + 1, b)
    else
      f(sum, 1, b + 1)
  }
}
