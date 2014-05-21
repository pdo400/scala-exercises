package patrickw.euler

import patrickw.util.{Palindromes, DigitList}
import Palindromes.isPalindrome

object P036 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 6 else args(0).toInt))

  def f(n: Int): Int =
    Palindromes(0 to 9)
      .tail
      .take(n)
      .flatten
      .filterNot(_.head % 2 == 0)
      .map(p => DigitList.unapply[Int](p).get)
      .filter(p => isPalindrome(Integer.toString(p, 2)))
      .sum
}
