package patrickw.euler

import patrickw.util.Permutation

object P024 {
  val digits = 0 to 9

  def main(args: Array[String]) =
    println(Permutation(digits, digits.size, (if (args.isEmpty) 1000000 else args(0).toInt) - 1).mkString)
}
