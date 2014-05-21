package patrickw.euler

import patrickw.util.Phi

object P072 {
  def main(args: Array[String]) =
    println(f(if (args.isEmpty) 1000000 else args(0).toLong))

  def f(to: Long): Long =
    (2L to to view) map (n => Phi(n)) sum
}
