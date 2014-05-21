package patrickw.util

import collection.generic.CanBuildFrom

object PowerSet {
  def apply[A, T[A] <: Traversable[A]](ta: T[A])(implicit bf: CanBuildFrom[T[A], A, T[A]]): Stream[T[A]] = {
    val b = bf()
    var i = 0

    BitMask.all(ta.size)
      .map (bits => { b.clear(); i = 0; ta.foreach(a => { if (bits.contains(i)) b += a; i = i + 1 }); b.result() })
  }
}
