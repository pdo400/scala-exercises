package patrickw.euler

import patrickw.util.{DigitList, Primes, Permutations}

object P043 {
  def main(args: Array[String]) =
    println(f().sum)

  def f(permutations: Seq[Seq[Int]] = Permutations(0 to 9, 3, false), divisors: Seq[Int] = Primes.primes.takeWhile(_ <= 17).reverse, acc: Seq[Int] = Nil): Stream[Long] =
    divisors match {
      case Seq() => permutations match {
        case Seq() => Stream.Empty
        case Seq(p, ps @ _*) => p match {
          case Seq(0, _*) => f(ps, divisors, acc)
          case _ => DigitList.unapply[Long](p ++ acc).get #:: f(ps, divisors, acc)
        }
      }
      case Seq(d, ds @ _*) => permutations match {
        case Seq() => Stream.Empty
        case Seq(p, ps @ _*) => {
          val (newAcc, newRem) = acc match {
            case Seq() => p splitAt 3
            case _ => (p.head +: acc, p.tail)
          }
          DigitList.unapply[Int](newAcc take 3).get % d match {
            case 0 => f(Permutations(newRem, 1, false), ds, newAcc) #::: f(ps, divisors, acc)
            case _ => f(ps, divisors, acc)
          }
        }
      }
    }
}
