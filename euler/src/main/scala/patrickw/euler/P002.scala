package patrickw.euler

import patrickw.util.Fibonaccis

object P002 {
  def main(args: Array[String]) =
    println(f(Fibonaccis(), if (args.isEmpty) 4000000 else args(0).toInt))

  def f(seq: Seq[Int], max: Int, acc: Int = 0): Int =
    if (seq.head >= max)
      acc
    else
      f(seq.tail, max, if (seq.head % 2 == 0) acc + seq.head else acc)

}
