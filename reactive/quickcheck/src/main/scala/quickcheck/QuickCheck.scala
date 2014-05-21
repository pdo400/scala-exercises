package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.annotation.tailrec
import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  val emptyHc = HC(empty, 0)

  lazy val genHeap: Gen[H] = oneOf(empty, for { a <- arbitrary[Int]; h <- genHeap } yield insert(a, h))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genHeapWithCount: Gen[HC] = oneOf(emptyHc, for { op <- genInsert; hc <- genHeapWithCount } yield op(hc))
  implicit lazy val arbHeapWithCount: Arbitrary[HC] = Arbitrary(genHeapWithCount)

  lazy val genDeleteMin = value(DeleteMin)
  lazy val genInsert = for (x <- arbitrary[Int]) yield Insert(x)
  lazy val genMeld = for (hc <- arbitrary[HC]; b <- arbitrary[Boolean]) yield Meld(hc, b)

  lazy val genOp = oneOf(genDeleteMin, genInsert, genMeld)
  implicit lazy val arbOp: Arbitrary[Op] = Arbitrary(genOp)

  case class HC(h: H, c: Int)

  sealed trait Op {
    def apply(hc: HC): HC
  }

  case object DeleteMin extends Op {
    def apply(hc: HC) = HC(deleteMin(hc.h), hc.c - 1)
  }

  case class Insert(x: Int) extends Op {
    def apply(hc: HC) = HC(insert(x, hc.h), hc.c + 1)
  }

  case class Meld(other: HC, otherLeft: Boolean) extends Op {
    def apply(hc: HC) = HC(if (otherLeft) meld(other.h, hc.h) else meld(hc.h, other.h), hc.c + other.c)
  }

  def toHeap(l: List[Int]) = l.foldLeft (empty) ((h, a) => insert(a, h))

  def toList(h: H): List[Int] = {
    @tailrec
    def toList(l: List[Int], h: H): List[Int] =
      if (isEmpty(h)) l.reverse else toList(findMin(h) :: l, deleteMin(h))

    toList(Nil, h)
  }

  @tailrec
  final def equiv(h: H, l: List[Int]): Boolean = l match {
    case head :: tail => !isEmpty(h) && findMin(h) == head && equiv(deleteMin(h), tail)
    case Nil => isEmpty(h)
  }

  def isOrdered(h: H) = {
    @tailrec
    def isOrdered(h: H, prev: Int): Boolean =
      if (isEmpty(h)) true
      else {
        val next = findMin(h)
        prev <= next && isOrdered(deleteMin(h), next)
      }

    isEmpty(h) || isOrdered(h, findMin(h))
  }

  def isValid(hc: HC) = {
    @tailrec
    def validate(h: H, prev: Int, count: Int): Option[Int] =
      if (isEmpty(h)) Some(count) else {
        val next = findMin(h)
        if (prev <= next) validate(deleteMin(h), next, count + 1) else None
      }

    if (isEmpty(hc.h)) hc.c == 0 else validate(hc.h, findMin(hc.h), 0).exists(_ == hc.c)
  }

  @tailrec
  final def checkOps(hc: HC, ops: List[Op]): Boolean = ops match {
    case head :: tail => head match {
      case DeleteMin => if (hc.c == 0) Try(head(hc)).isFailure else checkOps(head(hc), tail)
      case _ => checkOps(head(hc), tail)
    }
    case Nil => isValid(hc)
  }

  property("min1") = forAll { a: Int => findMin(insert(a, empty)) == a }

  property("orderedInsert") = forAll { h: H => isOrdered(h) }

  property("orderedMeld") = forAll { (l: H, r: H) => isOrdered(meld(l, r)) }

  property("sortedInput") = forAll { l: List[Int] => equiv(toHeap(l), l.sorted(ord)) }

  property("sortedInputMeld") = forAll {
    (l: List[Int], r: List[Int]) => equiv(meld(toHeap(l), toHeap(r)), (l ::: r).sorted(ord)) }

  property("ops") = forAll { l: List[Op] => checkOps(emptyHc, l) }
}
