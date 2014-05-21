package patrickw.euler

import patrickw.util.Primes.primes
import patrickw.util.Fractran
import patrickw.util.Fractran.Operation

object P308 {
  val l = List[(Int, Int)]((17, 91), (78, 85), (19, 51), (23, 38), (29, 33), (77, 29), (95, 23), (77, 19), (1, 17), (11, 13), (13, 11), (15, 2), (1, 7), (55, 1))

  def main(args: Array[String]) =
    println(f(primes((if (args.isEmpty) 10001 else args(0).toInt) - 1)))

  def f(p: Int, transitions: Stream[(Operation, Map[Int, Int])] = Fractran(2, l), acc: BigInt = 1): BigInt = {
    if (transitions.head._2.size == 1 && transitions.head._2.get(2) == Some(p))
      acc
    else
      f(p, transitions.tail, acc + 1)
  }
}
