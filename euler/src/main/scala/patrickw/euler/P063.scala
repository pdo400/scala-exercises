package patrickw.euler

import patrickw.util.LogFloor


object P063 {

  def main(args: Array[String]) = {
    println(f())
  }

  def f(): Int = {
    (1 to 9).flatMap(x => g[BigInt](x)).toSet.size
  }

  def g[A](x: Int)(implicit num: Integral[A]): List[A] = {
    import num._
    val xa = num.fromInt(x)

    def h(n: A, exp: Int, acc: List[A]): List[A] = {
      LogFloor(n, 10) + 1 match {
        case digits if digits == exp => {
          //println((x, exp, n))
          h(xa * n, exp + 1, n :: acc)
        }
        case _ => acc
      }
    }
    h(xa, 1, Nil)
  }
}
