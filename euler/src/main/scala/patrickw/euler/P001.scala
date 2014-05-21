package patrickw.euler

object P001 {
  def main(args: Array[String]) =
    println(f(1, if (args.isEmpty) 1000 else args(0).toInt))

  def f(start: Int, stop: Int, acc: Int = 0): Int =
    if (start >= stop)
      acc
    else
      f(start + 1, stop, if (start % 3 == 0 || start % 5 == 0) acc + start else acc)

}
