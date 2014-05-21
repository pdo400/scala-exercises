package patrickw.util

object Primes {
  private val InputFile = "input/primes.txt"

  val primes: Stream[Int] = generator()

  def fromFile(): Iterator[Int] =
    io.Source.fromInputStream(getClass.getResourceAsStream(InputFile)).getLines() map (s => s.toInt)

  def generator(): Stream[Int] = {
    def f(previous: => Stream[Int]) = {
      def from(n: Int): Stream[Int] = {
        if (isPrime(n, previous))
          n #:: from(n + 2)
        else
          from(n + 2)
      }
      2 #:: from(3)
    }

    lazy val primes: Stream[Int] = f(primes)
    primes
  }

  def sieveOfEratosthenes(to: Int): Stream[Int] = {
    def toIndex(n: Int) = (n - 3) / 2

    val isComposite = new Array[Boolean](math.max(toIndex(to) + 1, 0))

    def from(n: Int): Stream[Int] = {
      val index = toIndex(n)

      if (n > to)
        Stream.empty
      else if (!isComposite(index)) {
        Stream.from(index + n, n).takeWhile(_ < isComposite.length).foreach(i => isComposite(i) = true)
        n #:: from(n + 2)
      } else
        from(n + 2)
    }

    if (to < 2)
      Stream.empty
    else
      2 #:: from(3)
  }

  def isPrime(n: Int, primes: Seq[Int] = primes): Boolean = {
    def f(primes: Seq[Int]): Boolean =
      primes.head match {
        case p if p * p > n => true
        case p if n % p == 0 => false
        case _ => f(primes.tail)
      }

    if (n < 2) false else f(primes)
  }

  def apply(n: Int): Int =
    primes(n - 1)

  def unapply(n: Int): Option[Int] = {
    def f(primes: Seq[Int] = primes, acc: Int = 1): Option[Int] =
      primes.head match {
        case p if p == n => Some(acc)
        case p if n % p == 0 => None
        case _ => f(primes.tail, acc + 1)
      }

    if (n < 2) None else f()
  }
}
