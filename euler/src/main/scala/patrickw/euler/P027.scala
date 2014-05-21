package patrickw.euler

import patrickw.util.Primes

object P027 {
  val ordering = Ordering.Tuple3[Int, Int, Int]

  def main(args: Array[String]) = {
    val v = f(if (args.isEmpty) 1000 else args(0).toInt)
    println("ab = " + v._2 * v._3)
    println("a = " + v._2)
    println("b = " + v._3)
    println("count = " + v._1)
  }

  def f(maxAbs: Int) = {
    def g(stream: Stream[(Int, Int, Int)] = from(maxAbs), acc: (Int, Int, Int) = (0, 0, 0)): (Int, Int, Int) =
      stream match {
        case Stream.Empty => acc
        case #::(head, tail) => g(tail, ordering.max(head, acc))
      }
    g()
  }

  def from(maxAbs: Int, primes: Stream[Int] = Primes.primes): Stream[(Int, Int, Int)] =
    if (primes.head >= maxAbs)
      Stream.Empty
    else
      ((-maxAbs to maxAbs) map (a => (count(a, primes.head), a, primes.head)) max ordering) #:: from(maxAbs, primes.tail)

  def count(a: Int, b: Int, n: Int = 0): Int = {
    Primes.unapply(n * n + a * n + b) match {
      case Some(p) => count(a, b, n + 1)
      case None => n
    }
  }
}
