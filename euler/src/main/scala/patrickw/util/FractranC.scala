package patrickw.util

class FractranC(var state: Option[Array[Int]],
                operations: Traversable[(Array[Int], Array[Int])])
{
  val s = state.get
  var l: Traversable[(Array[Int], Array[Int])] = _
  var continue: Boolean = _
  var continue2: Boolean = _
  var i: Int = _
  var n: Array[Int] = _
  var d: Array[Int] = _

  def next() = {
    l = operations
    continue = true
    while (continue) {
      n = l.head._1
      d = l.head._2
      i = 0
      continue2 = true
      while (continue2) {
        if (i == s.length) {
          i = 0
          while (i < s.length) {
            s(i) += n(i) - d(i)
            i += 1
          }
          continue2 = false
          continue = false
        } else {
          if (d(i) > s(i)) {
            l = l.tail
            continue2 = false
          } else {
            i += 1
          }
        }
      }
    }
  }
}

object FractranC {
  def apply(initialState: Int, operations: Traversable[(Int, Int)]): FractranC = {
    val size = Primes.unapply((FactorStream(initialState) ++ (operations flatMap (p => FactorStream(p._1) ++ FactorStream(p._2))) max).toInt).get
    new FractranC(Some(RegisterSet(initialState, size)), operations map (p => (RegisterSet(p._1, size), RegisterSet(p._2, size))))
  }
}
