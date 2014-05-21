package patrickw.util

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 11/9/12
 * Time: 9:14 AM
 */
object ArrayIndexer {
  def apply(di: Int, dj: Int): ArrayIndexer2 =
    new ArrayIndexer2(di, dj)

  def apply(dimensions: (Int, Int)): ArrayIndexer2 =
    apply(dimensions._1, dimensions._2)
}

class ArrayIndexer2(di: Int, dj: Int) {
  def apply(i: Int, j: Int): Int =
    i * di + j

  def apply(position: (Int, Int)): Int =
    apply(position._1, position._2)

  def apply[A](i: Int, j: Int, array: Array[A]): A =
    array(apply(i, j))

  def apply[A](position: (Int, Int), array: Array[A]): A =
    array(apply(position))
}
