package patrickw.euler

import patrickw.util._

object P078 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1000000 else args(0).toInt))

  def f(mod: Int) = {
    val pMod = Partition.memoized(IntegralMod(mod))
    Iterator.from(1).find(pMod(_) == 0)
  }
}
