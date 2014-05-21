package patrickw.euler

object P018 {
  val input = "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

  def main(args: Array[String]) =
    println(f((if (args.isEmpty) input else args(0)) split ('\n') map (s => s.split(' ') map (n => n.toInt))))

  def f(triangle: Traversable[Array[Int]], acc: IndexedSeq[Int] = Array[Int](0)): Int = {
    //acc foreach (x => print(x + " " * (5 - math.log10(x).floor.intValue))); println()
    if (triangle.isEmpty)
      acc max
    else if (acc.isEmpty)
      f(triangle.tail, triangle.head)
    else {
      f(triangle.tail,
        for (i <- 0 until triangle.head.size) yield
          triangle.head(i) + (i match {
            case 0 => acc(i)
            case i if i == acc.length => acc(i - 1)
            case i => acc(i) max acc(i - 1)
          }))
    }
  }
}
