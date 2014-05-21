package patrickw.util

import java.lang.IllegalArgumentException

object LetterValues {
  def apply(seq: Seq[Char]): Seq[Int] = {
    def exception(c: Char): IllegalArgumentException =
      new IllegalArgumentException("Invalid character: decimal " + c.toInt)

    seq map { c =>
      c match {
        case v if v < 'A' => throw exception(v)
        case v if v <= 'Z' => v - 'A' + 1
        case v if v < 'a' => throw exception(v)
        case v if v <= 'z' => v - 'a' + 1
        case v => throw exception(v)
      }
    }
  }

  def unapply(seq: Seq[Int]): Option[Seq[Char]] =
    Some(seq map { c =>
      c match {
        case v if v < 1 => return None
        case v if v <= 26 => (v + 'a' - 1).toChar
        case _ => return None
      }
    })
}
