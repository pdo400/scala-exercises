package patrickw.euler

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap

object P031 {
  val denominations = List(1, 2, 5, 10, 20, 50, 100, 200)

  def main(args: Array[String]) =
    println(f()(if (args.isEmpty) 200 else args(0).toInt))

  def f(): Seq[Long] = {
    val buf = ArrayBuffer[HashMap[Int, Long]]()
    def g(n: Int = 0): Stream[Long] = {
      def h(l: List[Int] = denominations, last: Int = 0): Stream[Long] =
        l match {
          case Nil => buf(n)(last) #:: g(n + 1)
          case d :: ds => {
            buf(n) += d -> (for (i <- 0 to n / d) yield buf(n - i * d)(last)).sum
            h(ds, d)
          }
        }
      buf += HashMap[Int, Long](0 -> (if (n == 0) 1L else 0L))
      h()
    }
    g()
  }

  // Much slower than f
  def g(n: Int, l: List[Int] = denominations reverse): Long =
    if (n == 0) 1 else
      l match {
        case d :: Nil => if (n % d == 0) 1 else 0 // don't branch on the last coin
        case d :: ds => (for (i <- 0 to n / d) yield g(n - i * d, ds)) sum
        case Nil => if (n == 0) 1 else 0 // won't happen if denominations is non-empty
      }
}
