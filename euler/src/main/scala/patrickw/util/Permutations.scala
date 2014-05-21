package patrickw.util

object Permutations {
  def apply[T](seq: Seq[T]): Stream[Seq[T]] =
    apply(seq, seq.size)

  def apply[T](seq: Seq[T], pick: Int, truncate: Boolean = true): Stream[Seq[T]] = {
    val max = Factorial(seq.size) / Factorial(seq.size - pick)
    def f(from: Int = 0): Stream[Seq[T]] =
      if (from == max)
        Stream.Empty
      else
        Permutation(seq, pick, from, truncate) #:: f(from + 1)

    f()
  }
}
