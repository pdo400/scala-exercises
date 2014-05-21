package patrickw.euler

object P014 {
  def main(args: Array[String]) =
    println(f((if (args.isEmpty) 1000000 else args(0).toInt) - 1))


  def f(max: Int) = {
    val a = new Array[Int](10 * max)
    def g(n: Long): Int =
      if (n >= a.length)
        1 + (if (n % 2 == 0) g(n / 2) else g(3 * n + 1))
      else
        a(n.toInt) match {
          case 0 => {
            a(n.toInt) = 1 + (if (n % 2 == 0) g(n / 2) else g(3 * n + 1))
            a(n.toInt)
          }
          case x => x
        }

    a(1) = 1
    var maxN = 0
    var maxLength = 0
    for (n <- 1 to max)
      if (g(n) > maxLength) {
        maxLength = g(n)
        maxN = n
      }
    (maxN, maxLength)
  }
}
