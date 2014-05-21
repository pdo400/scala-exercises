package patrickw.util

object Rotations {
  def apply[T](seq: Seq[T]): Stream[Seq[T]] = {
    val size = seq.size
    def f(acc: Int = 0): Stream[Seq[T]] =
      if (acc == seq.size)
        Stream.Empty
      else {
        (seq.view(acc, size) ++ seq.view(0, acc)) #:: f(acc + 1)
      }

    f()
  }
}
