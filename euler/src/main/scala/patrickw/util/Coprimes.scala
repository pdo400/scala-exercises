package patrickw.util

import collection.mutable

object Coprimes {
  private val oddOddGenerator = (3, 1)
  private val evenOddGenerator = (2, 1)

  def all(): Iterator[(Int, Int)] = using(evenOddGenerator, oddOddGenerator)
  def oddOdd(): Iterator[(Int, Int)] = using(oddOddGenerator)
  def evenOdd(): Iterator[(Int, Int)] = using(evenOddGenerator)

  private def using(generators: (Int, Int)*): Iterator[(Int, Int)] = {
    val ord = Ordering.Tuple2[Int, Int].reverse
    val queue = new mutable.PriorityQueue[(Int, Int)]()(ord)

    def next(): (Int, Int) = {
      val (m, n) = queue.dequeue()
      queue.enqueue((2 * m - n, m), (2 * m + n, m), (m + 2 * n, n))
      (m, n)
    }

    generators.foreach(g => queue.enqueue(g))
    Iterator.continually(next())
  }
}
