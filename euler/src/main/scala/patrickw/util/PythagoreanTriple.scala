package patrickw.util


object PythagoreanTriple {
  def from(orderedPair: (Int, Int)): (Int, Int, Int) =
    orderedPair match {
      case (m, n) if m > n => (m * m - n * n, 2 * m * n, m * m + n * n)
      case (m, n) if m < n => from(n, m)
      case _ => sys.error("m, n must not be equal in ordered pair (m, n)")
    }

  def primitives(): Iterator[(Int, Int, Int)] = Coprimes.evenOdd().map(from)

  def primitivesWithGenerator(): Iterator[((Int, Int), (Int, Int, Int))] = Coprimes.evenOdd().map(p => (p, from(p)))
}
