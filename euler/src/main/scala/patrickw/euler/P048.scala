package patrickw.euler

object P048 {
  def main(args: Array[String]) =
    println(f()(if (args.isEmpty) 1000 else args(0).toInt))

  def f(digits: Int = 10): Stream[BigInt] = {
    val mod = BigInt(10).pow(digits)

    def g(n: BigInt = 1, acc: BigInt = 0): Stream[BigInt] =
      acc #:: g(n + 1, (acc + n.modPow(n, mod)) % mod)

    g()
  }
}
