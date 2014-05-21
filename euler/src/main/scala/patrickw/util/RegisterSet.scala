package patrickw.util

object RegisterSet {
  def apply(state: Long, size: Int): Array[Int] = {
    val m = FactorMap(state)
    val a = new Array[Int](size)
    for (n <- 0 until a.size)
      a(n) = m.getOrElse(Primes(n + 1), 0)
    a
  }

  def unapply(registerSet: Array[Int]): Option[Long] = {
    var acc = 1
    for (n <- 0 until registerSet.size)
      for (e <- 0 until registerSet(n))
        acc *= Primes(n + 1)
    Some(acc)
  }
}



