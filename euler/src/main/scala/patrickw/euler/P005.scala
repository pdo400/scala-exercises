package patrickw.euler

import patrickw.util.Lcm

object P005 {
  def main(args: Array[String]) =
    println(Lcm(1L to (if (args.isEmpty) 20 else args(0).toInt): _*))
}
