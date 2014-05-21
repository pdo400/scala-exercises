package patrickw.util

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 10/10/12
 * Time: 4:19 PM
 */
object ProductStream {
  def apply[A](as: Seq[A]) = {
    as.toStream
  }

  def apply[A, B](as: Seq[A], bs: Seq[B]): Stream[(A, B)] = {
    def f(ra: Seq[A], rb: Seq[B], depth: Int, stream: Stream[(A, B)]): Stream[(A, B)] = {
      stream match {
        case head #:: tail => head #:: f(ra, rb, depth, tail)
        case _ =>
          if (ra.isEmpty && rb.isEmpty)
            Stream.empty
          else
            f(if (ra.isEmpty) ra else ra.tail, if (rb.isEmpty) rb else rb.tail, depth + 1, apply(as, bs, depth, depth + 1))
      }
    }
    f(as, bs, 0, Stream.empty)
  }

  def apply[A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C]): Stream[(A, B, C)] = {
    def f(ra: Seq[A], rb: Seq[B], rc: Seq[C], depth: Int, stream: Stream[(A, B, C)]): Stream[(A, B, C)] = {
      stream match {
        case head #:: tail => head #:: f(ra, rb, rc, depth, tail)
        case _ =>
          if (ra.isEmpty && rb.isEmpty && rc.isEmpty)
            Stream.empty
          else
            f(if (ra.isEmpty) ra else ra.tail, if (rb.isEmpty) rb else rb.tail, if (rc.isEmpty) rc else rc.tail, depth + 1, apply(as, bs, cs, depth, depth + 1))
      }
    }
    f(as, bs, cs, 0, Stream.empty)
  }

  def apply[A](as: Seq[A], minDepth: Int, maxDepth: Int): Stream[A] =
    as.toStream.take(maxDepth).drop(minDepth)

  def apply[A, B](as: Seq[A], bs: Seq[B], minDepth: Int, maxDepth: Int): Stream[(A, B)] =
    as.toStream.take(maxDepth).zipWithIndex.flatMap { case (a, index) =>
      ProductStream(bs, if (index < minDepth) minDepth else 0, maxDepth).map(b => (a, b))
    }

  def apply[A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C], minDepth: Int, maxDepth: Int): Stream[(A, B, C)] =
    as.toStream.take(maxDepth).zipWithIndex.flatMap { case (a, index) =>
      ProductStream(bs, cs, if (index < minDepth) minDepth else 0, maxDepth).map { case (b, c) => (a, b, c) }
    }

}
