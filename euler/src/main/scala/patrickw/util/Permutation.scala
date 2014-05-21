package patrickw.util

object Permutation {
  def apply[T](seq: Seq[T], pick: Int, lex: Int, truncate: Boolean = true): Seq[T] = {
    val size = seq.size
    val v = apply(seq, Factoradic(Factorial(size - pick) * lex))

    if (truncate && pick < size)
      v.take(pick)
    else
      v
  }

  def apply[T](seq: Seq[T], factoradic: Seq[Int]): Seq[T] = {
    def f(seq: Seq[T], factoradic: Seq[Int]): Stream[T] =
      if (seq.size > factoradic.size)
        seq.head #:: f(seq.tail, factoradic)
      else
        factoradic match {
          case Nil => Stream.Empty
          case head :: tail => {
            val (take, drop) = seq.splitAt(head)
            drop.head #:: f(take ++ drop.tail, tail)
          }
        }

    f(seq, factoradic)
  }
}
