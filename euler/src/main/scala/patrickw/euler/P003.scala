package patrickw.euler

import patrickw.util.FactorStream

object P003 {
  def main(args: Array[String]) =
    println(FactorStream(if (args.isEmpty) 600851475143L else args(0).toLong) max)
}
