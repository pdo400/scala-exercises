package patrickw.tonymorris.optional

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 10/7/12
 * Time: 11:51 AM
 */
object TestOptional {
  import Optional._

  implicit def ArbitraryOptional[A](implicit a: Arbitrary[A]): Arbitrary[Optional[A]] =
    Arbitrary(arbitrary[Option[A]] map fromOption)

  val prop_map = forAll ((o: Optional[Int], f: Int => String) =>
    (o map f).toOption == (o.toOption map f))

  val prop_get = forAll((o: Optional[Int]) =>
    o.isDefined ==>
      (o.get == o.toOption.get))

  val prop_flatMap = forAll((o: Optional[Int], f: Int => Optional[String]) =>
    (o flatMap f).toOption == (o.toOption flatMap (f(_).toOption)))

  val prop_mapAgain = forAll ((o: Optional[Int], f: Int => String) =>
    (o mapAgain f).toOption == (o map f).toOption)

  val prop_getOrElse = forAll ((o: Optional[Int], n: Int) =>
    (o getOrElse n) == (o.toOption getOrElse n))

  val prop_filter = forAll ((o: Optional[Int], f: Int => Boolean) =>
    (o filter f).toOption == (o.toOption filter f))

  val prop_exists = forAll ((o: Optional[Int], f: Int => Boolean) =>
    (o exists f) == (o.toOption exists f))

  val prop_forall = forAll ((o: Optional[Int], f: Int => Boolean) =>
    (o forall f) == (o.toOption forall f))

  val prop_foreach = forAll ((o: Optional[Int], f: Int => Unit, n: Int) => {
    var x: Int = n
    var y: Int = x

    o foreach (t => x = x + t)
    o.toOption foreach (t => y = y + t)

    x == y
  })

  val prop_isDefined = forAll ((o: Optional[Int]) =>
    (o.isDefined) == (o.toOption.isDefined))

  val prop_isEmpty = forAll ((o: Optional[Int]) =>
    o.isEmpty == o.toOption.isEmpty)

  val prop_orElse = forAll ((o: Optional[Int], p: Optional[Int]) =>
    (o orElse p).toOption == (o.toOption orElse p.toOption))

  val prop_toLeft = forAll ((o: Optional[Int], n: Int) =>
    (o toLeft n) == (o.toOption toLeft n))

  val prop_toRight = forAll ((o: Optional[Int], n: Int) =>
    (o toRight n) == (o.toOption toRight n))

  val prop_toList = forAll ((o: Optional[Int]) =>
    o.toList == o.toOption.toList)

  val prop_iterator = forAll ((o: Optional[Int]) =>
    o.iterator sameElements o.toOption.iterator)

  // *** READ THIS COMMENT FIRST ***
  // Note that scala.Option has no such equivalent to this method
  // Therefore, reading this test may give away clues to how it might be solved.
  // If you do not wish to spoil it, look away now and follow the
  // instruction in the Exercise comment.
  val prop_applic = forAll ((o: Optional[Int => String], p: Optional[Int]) =>
    (p applic o).toOption ==
    (for(f <- o.toOption;
        n <- p.toOption)
    yield f(n)))

  val props =
    List(
      prop_map,
      prop_get,
      prop_flatMap,
      prop_mapAgain,
      prop_getOrElse,
      prop_filter,
      prop_exists,
      prop_forall,
      prop_foreach,
      prop_isDefined,
      prop_isEmpty,
      prop_orElse,
      prop_toLeft,
      prop_toRight,
      prop_toList,
      prop_iterator,
      prop_applic
    )

  /*
  $ scala -classpath .:scalacheck_2.8.0-1.8-SNAPSHOT.jar TestOptional
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  + OK, passed 100 tests.
  */
  def main(args: Array[String]) {
    props foreach (_.check)
  }
}
