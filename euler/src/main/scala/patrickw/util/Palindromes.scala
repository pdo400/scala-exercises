package patrickw.util

object Palindromes {
  def apply[T](symbols: Traversable[T]): Stream[Seq[Seq[T]]] = {
    def f(acc: Seq[Seq[T]] = Seq(Seq())): Stream[Seq[Seq[T]]] = {
      val newAcc = acc.head.size % 2 match {
        case 0 => acc flatMap (seq => {
          val (s, _, e) = split(seq)
          symbols map (symbol => s ++ (symbol +: e))
        })
        case _ => acc map (seq => {
          val (s, m, e) = split(seq)
          s ++ (m.get +: m.get +: e)
        })
      }
      newAcc #:: f(newAcc)
    }
    Nil #:: f()
  }

  def isPalindrome[T](seq: Seq[T]): Boolean = {
    val (s, _, e) = split(seq)
    s == e.reverse
  }

  def split[T](s: Seq[T]): (Seq[T], Option[T], Seq[T]) = {
    val (take, drop) = s.splitAt(s.size / 2)
    s.size % 2 match {
      case 0 => (take, None, drop)
      case _ => (take, Some(drop.head), drop.tail)
    }
  }
}
