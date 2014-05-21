package patrickw.euler

import patrickw.util._

object P076 {
  def main(args: Array[String]) =
    println(Partition[BigInt](if (args.isEmpty) 100 else args(0).toInt) - 1)
}
