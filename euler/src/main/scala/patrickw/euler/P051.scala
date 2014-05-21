package patrickw.euler

import patrickw.util.combinator.Memoize
import patrickw.util.{BitMask, DigitList, Primes}

object P051 {
  def main(args: Array[String]) {
    println(f(if (args.isEmpty) 8 else args(0).toInt))
  }

  def f(count: Int) : (String, Int, Int) = {
    val m = scala.collection.mutable.Map.empty[Int, (Int, Int)]
    val masks = Memoize((n: Int) => BitMask.all(n).drop(1))

    Primes.primes.map(
      p => {
        val digits = DigitList(p).zipWithIndex

        masks(digits.size).foldLeft((0, 0, 0))(
          (best, mask) =>
            digits.foldLeft[Option[(Option[Int], Int)]](Some(None, 0)) {
              case (None, _) => None
              case (Some((requiredDigit, acc)), (digit, index)) =>
                if(mask.contains(index))
                  if(requiredDigit.isEmpty)
                    Some(Some(digit), 0x10 * acc + 0xa)
                  else if(requiredDigit.exists(_ == digit))
                    Some(requiredDigit, 0x10 * acc + 0xa)
                  else
                    None
                else
                  Some(requiredDigit, 0x10 * acc + digit)
            } map {
              case (_, key) =>
                val x = m.get(key).map(v => (v._1, v._2 + 1)).getOrElse((p, 1))
                m.update(key, x)
                if (x._2 > best._3)
                  (key, x._1, x._2)
                else
                  best
            } getOrElse(best)
        )
      }
    ). filter (_._3 >= count). map (v => (v._1.toHexString, v._2, v._3)). head
  }
}
