package patrickw.euler

object P029 {
  // Got tired and cheesed out

  def main(args: Array[String]) = {
    val s = scala.collection.mutable.HashSet[BigInt]()
    for (i <- 2 to 100) {
      val bi = BigInt(i)
      for (j <- 2 to 100)
        s += bi.pow(j)
    }
    println(s.size)
  }

  //  def main(args: Array[String]) =
  //    println(f(if (args.isEmpty) 100 else args(0).toInt))
  //
  //  def f(max: Int, i: Int = 2, acc: Int = 0): Int = {
  //    if (i > max)
  //      acc
  //    else {
  //      0
  ////      val v = Gcd(FactorMap(i).values.toSeq: _*) match {
  ////        case 1 => max - 1
  ////        case p => max - max / p
  ////      }
  ////      println(i + " : " + v)
  ////      f(max, i + 1, acc + v)
  //    }
  //  }
}
