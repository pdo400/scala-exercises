package patrickw.euler

object P026 {
  def main(args: Array[String]) =
    println((1 until (if (args.isEmpty) 1000 else args(0).toInt)) map (x => (f(x), x)) max Ordering.Tuple2[Int, Int])

  def f(n: Int, l: List[Int] = List(1)): Int =
    (10 * l.head) % n match {
      case 0 => 0
      case r => l.indexOf(r) match {
        case -1 => f(n, r :: l)
        case i => i + 1
      }
    }
}
