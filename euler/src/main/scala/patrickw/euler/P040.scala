package patrickw.euler

import patrickw.util.DigitList

object P040 {
  def main(args: Array[String]) = {
    val digits = (0 to 6) map (n => digit(BigInt(10).pow(n)))
    println(digits)
    println(digits product)
  }

  def digit(n: BigInt, nextLength: Int = 1): Int = {
    n - nextLength * ofLength(nextLength) match {
      case x if x == 0 => 9
      case x if x > 0 => digit(x, nextLength + 1)
      case _ => {
        val num = BigInt(10).pow(nextLength - 1) + (n - 1) / nextLength
        val dig = ((n - 1) % nextLength).intValue
        val l = DigitList(num)
        l(dig).intValue
      }
    }
  }

  def ofLength(n: Int): Int =
    (BigInt(10).pow(n) - BigInt(10).pow(n - 1)).intValue

}
