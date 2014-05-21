package patrickw.euler

object P004 {
  def main(args: Array[String]) =
    println(findPalindrome(999, 100))

  def findPalindrome(maxFactor: Int, minFactor: Int) =
    f(maxFactor, maxFactor, minFactor, 0)

  def f(big: Int, little: Int, minFactor: Int, found: Int): Int =
    if (big < minFactor) {
      found
    }
    else if (little < 0)
      f(big - 1, big - 1, minFactor, found)
    else {
      val n = little * big
      if (n <= found)
        f(big - 1, big - 1, minFactor, found)
      else if (n.toString == n.toString.reverse)
        f(big - 1, big - 1, little, n)
      else
        f(big, little - 1, minFactor, found)
    }
}
