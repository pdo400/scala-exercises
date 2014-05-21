package patrickw.util

import combinator.{Memoize, Y}

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 5/30/12
 * Time: 6:53 AM
 */

object FactorSums {
  // This version should only be used for one-offs, as it rebuilds the Memoization on every call
  def apply(n: Int): Map[Int, Set[Int]] =
    Y(Memoize[Int, Map[Int, Set[Int]]] _ compose endomorphic)(n)

  def memoized: Int => Map[Int, Set[Int]] =
    Y(Memoize[Int, Map[Int, Set[Int]]] _ compose endomorphic)

  def endomorphic(recur: Int => Map[Int, Set[Int]]): Int => Map[Int, Set[Int]] = n => {
    (Divisors(n) foldLeft Map(1 -> Set(n))) {
      case (acc, d) => d match {
        case 1 => acc
        case `n` => acc
        case d => (recur(n / d) foldLeft acc) {
          case (acc, (len, sums)) => acc.updated(len + 1, sums.map(_ + d) ++ acc.getOrElse(len + 1, Set.empty))
        }
      }
    }
  }
}
