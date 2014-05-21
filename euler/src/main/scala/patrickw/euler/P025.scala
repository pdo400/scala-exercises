package patrickw.euler

import patrickw.util.Fibonaccis

object P025 {
  def main(args: Array[String]) =
    println(f((if (args.isEmpty) 1000 else args(0).toInt)))

  def f(digits: Int) = {
    val d = BigInt(10) pow (digits - 1)
    def g(seq: Seq[BigInt] = Fibonaccis(Numeric.BigIntIsIntegral), acc: Int = 1): Int =
      if (seq.head >= d)
        acc
      else
        g(seq.tail, acc + 1)

    g()
  }
}
