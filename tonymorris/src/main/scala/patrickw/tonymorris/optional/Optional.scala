package patrickw.tonymorris.optional

/*

  Below are 15 exercises. The task is to emulate the scala.Option API
  without using Some/None subtypes, but instead using a fold (called a
  catamorphism).

  A couple of functions are already done (map, get)
  to be used as an example. ScalaCheck tests are given below to
  verify the work. The desired result is to have all tests passing.

  The 15th exercise is not available in the existing Scala API so
  instructions are given in the comments.

  Revision History
  ================

  23/08/2010
  Initial revision

  ----------------

  23/08/2010
  Fixed prop_getOrElse. Thanks Michael Bayne.

  ----------------

  26/08/2010
  Add lazy annotation to orElse method.

*/


trait Optional[A] {
  // single abstract method
  def fold[X](some: A => X, none: => X): X

  import Optional._

  // Done for you.
  def map[B](f: A => B): Optional[B] =
    fold(f andThen some, none[B])

  // Done for you.
  // WARNING: undefined for None
  def get: A =
    fold(a => a, error("None.get"))

  // Exercise 1
  def flatMap[B](f: A => Optional[B]): Optional[B] =
    fold(f, none[B])

  // Exercise 2
  // Rewrite map but use flatMap, not fold.
  def mapAgain[B](f: A => B): Optional[B] =
    flatMap(f andThen some)

  // Exercise 3
  def getOrElse(e: => A): A =
    fold(a => a, e)

  // Exercise 4
  def filter(p: A => Boolean): Optional[A] =
    if (fold(p, true)) this else none

  // Exercise 5
  def exists(p: A => Boolean): Boolean =
    fold(p, false)

  // Exercise 6
  def forall(p: A => Boolean): Boolean =
    fold(p, true)

  // Exercise 7
  def foreach(f: A => Unit): Unit =
    fold(f, Unit)

  // Exercise 8
  def isDefined: Boolean =
    fold(_ => true, false)

  // Exercise 9
  def isEmpty: Boolean =
    !isDefined

  // Exercise 10
  def orElse(o: => Optional[A]): Optional[A] =
    if (isDefined) this else o

  // Exercise 11
  def toLeft[X](right: => X): Either[A, X] =
    fold(a => Left(a), Right(right))

  // Exercise 12
  def toRight[X](left: => X): Either[X, A] =
    fold(a => Right(a), Left(left))

  // Exercise 13
  def toList: List[A] =
    fold(a => a :: Nil, Nil)

  // Exercise 14
  def iterator: Iterator[A] =
    toList.iterator

  // Exercise 15 The Clincher!
  // Return a none value if either this or the argument is none.
  // Otherwise apply the function to the argument in some.
  // Don't be afraid to use functions you have written.
  // Better style, more points!
  def applic[B](f: Optional[A => B]): Optional[B] =
    flatMap(a => f.map(f => f(a)))

  // Utility
  def toOption: Option[A] = fold(Some(_), None)
}

object Optional {
  // Done for you
  def none[A]: Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = none
  }

  // Done for you
  def some[A](a: A): Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = some(a)
  }

  // Utility
  def fromOption[A](o: Option[A]): Optional[A] = o match {
    case None    => none
    case Some(a) => some(a)
  }
}
