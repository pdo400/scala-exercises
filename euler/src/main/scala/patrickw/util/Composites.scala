package patrickw.util

object Composites {
  private val MinComposite = 4

  def from(start: Int = 1, by: Int = 1): Iterator[Int] = {
    if (by < 1)
      error("by must be > 0")

    var n = if (start >= MinComposite) start else MinComposite + (by - (MinComposite - start) % by) % by
    var primes = Primes.primes dropWhile (_ < n)

    def next(): Int =
      n match {
        case x if x > primes.head =>
          primes = primes.tail
          next()
        case y if y == primes.head =>
          primes = primes.tail
          n += by
          next()
        case z if z < primes.head =>
          val t = n
          n += by
          t
      }

    Iterator.continually(next())
  }
}
