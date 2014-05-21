package patrickw.euler

import scala._
import patrickw.util.combinator.Memoize
import patrickw.util.Primes


object P060 {
  def main(args: Array[String]) {
    //
    // This actually finds the set with the smallest maximum element, not the set with the smallest sum.
    // However, the code could easily be used to find the set with the smallest sum by checking more primes.
    //
    println(f(if (args.isEmpty) 5 else args(0).toInt))
  }

  def f(length: Int) =
    g(length).
      filter { case (p, m) => m.contains(length) } .
      map { case (p, m) => val l = m.get(length).get.head; (l.sum, l) } .
      head

  def g(length: Int): Stream[(Int, Map[Int, List[List[Int]]])] = {
    val toString = Memoize((i: Int) => i.toString)

    def from(primes: Seq[Int], m: Map[Int, List[List[Int]]] = Map.empty): Stream[(Int, Map[Int, List[List[Int]]])] = {
      val p = primes.head
      val ps = toString(p)

      def isGood(p2: Int): Boolean = {
        val p2s = toString(p2)
        Primes.isPrime((ps + p2s).toInt) && Primes.isPrime((p2s + ps).toInt)
      }

      def make(from: Seq[List[Int]], to: List[List[Int]]): List[List[Int]] =
        from match {
          case Nil => to
          case head :: tail => make(tail, if (head.forall(isGood)) (p :: head) :: to else to)
        }

      def makeTwos(from: Stream[Int], to: List[List[Int]]): List[List[Int]] =
        from match {
          case Stream.Empty => to
          case head #:: tail => makeTwos(tail, if (isGood(head)) (p :: head :: Nil) :: to else to)
        }

      val newMap = {
        val twos = makeTwos(Primes.primes.takeWhile(_ < p), m.getOrElse(2, Nil))
        val notTwos = for {
          (i, lists) <- m
          next = make(lists, m.getOrElse(i + 1, Nil))
          if !next.isEmpty
        } yield (i + 1 -> next)

        if (twos.isEmpty) notTwos else notTwos + (2 -> twos)
      }

      (p, newMap) #:: from(primes.tail, newMap)
    }

    from(Primes.primes)
  }
}
