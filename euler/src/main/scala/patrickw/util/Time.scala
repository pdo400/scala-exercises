package patrickw.util

object Time {
  def apply[T](v: => T): (T, Long) = {
    val start = System.currentTimeMillis
    (v, System.currentTimeMillis - start)
  }
}
