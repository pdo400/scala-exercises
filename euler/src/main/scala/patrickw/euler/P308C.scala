package patrickw.euler

import patrickw.util.Primes
import patrickw.util.FractranC

object P308C {
  val l = List[(Int, Int)]((17, 91), (78, 85), (19, 51), (23, 38), (29, 33), (77, 29), (95, 23), (77, 19), (1, 17), (11, 13), (13, 11), (15, 2), (1, 7), (55, 1))

  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 10001 else args(0).toInt))

  def f(targetN: Int, fractranC: FractranC = FractranC(2, l)): Long = {
    val state = fractranC.state.get
    var acc: Long = 0
    var n = 1
    var p = Primes(n)
    var continue = true
    while (continue) {
      if (state(0) == p && (1 until state.length forall (i => state(i) == 0))) {
        println(n + " : " + acc)
        if (n == targetN) {
          continue = false
        } else {
          n += 1
          p = Primes(n)
        }
      } else {
        fractranC.next()
        acc += 1
      }
    }
    acc
  }
}
