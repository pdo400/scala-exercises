package patrickw.util

import collection.immutable.BitSet

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 5/25/12
 * Time: 12:52 AM
 */

object BitMask {
  def apply(mask: Int): BitSet =
    BitSet(0 to LogFloor(mask, 2) filter (n => (mask & 1 << n) > 0) : _*)

  def all(bits: Int): Stream[BitSet] = {
    require(bits >= 0, "bits must be >= 0")

    val stop = 1 << bits

    def from(n: Int): Stream[BitSet] = if (n >= stop) Stream.empty else apply(n) #:: from(n + 1)

    from(0)
  }
}
