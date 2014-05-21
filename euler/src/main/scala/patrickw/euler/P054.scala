package patrickw.euler

import patrickw.util.PokerDeck.Card

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 10/6/12
 * Time: 10:02 AM
 */
object P054 {
  private val InputFile = "input/P054.txt"

  def main(args: Array[String]) {
    println(f())
  }

  def f() : (Int, Int, Int) = {
    hands().foldLeft((0, 0, 0)) {
      case ((p1, p2, ties), (a, b)) => compare(a, b) match {
        case 1 => (p1 + 1, p2, ties)
        case -1 => (p1, p2 + 1, ties)
        case _ => (p1, p2, ties + 1)
      }
    }
  }

  def hands(): Iterator[(Seq[Card], Seq[Card])] = {
    io.Source.fromInputStream(getClass.getResourceAsStream(InputFile)).getLines
      .map(s => {
      val ss = s.splitAt(5 * (2 + 1))
      (hand(ss._1), hand(ss._2))
    })
  }

  def hand(s: String): Seq[Card] = {
    s.trim.split(" ").map(x => Card(x).get)
  }

  def compare(a: Seq[Card], b: Seq[Card]) : Int = {
    val hand1 = rank(a)
    val hand2 = rank(b)
    math.signum(
      hand1._1.handRank - hand2._1.handRank match {
        case 0 => hand1._2.zip(hand2._2).map { case (a, b) => a.rank.rank - b.rank.rank } find (_ != 0) getOrElse(0)
        case n => n
      })
  }

  def rank(hand: Seq[Card]): (HandRank, Seq[Card]) = {
    val sorted = hand.sortBy(card => card.rank.rank).reverse
    val straight = sorted.tail.foldLeft((true, sorted.head.rank.rank)) {
      case ((good, lastRank), card) => (good && card.rank.rank == lastRank - 1, card.rank.rank)
    }._1
    val flush = hand.forall(card => card.suit == hand.head.suit)

    if (straight && flush) {
      (HandRank.StraightFlush, sorted)
    } else if (flush) {
      (HandRank.Flush, sorted)
    } else if (straight) {
      (HandRank.Straight, sorted)
    } else {
      val grouped = hand.groupBy(card => card.rank).map { case (rank, cards) => (cards.length, rank, cards) }.toArray.sortBy(x => (x._1, x._2.rank)).reverse
      val handRank = grouped.head._1 match {
        case 4 => HandRank.FourOfAKind
        case 3 =>  grouped.tail.head._1 match {
          case 2 => HandRank.FullHouse
          case _ => HandRank.ThreeOfAKind
        }
        case 2 => grouped.tail.head._1 match {
          case 2 => HandRank.TwoPair
          case _ => HandRank.Pair
        }
        case _ => HandRank.HighCard
      }
      (handRank, grouped.flatMap(x => x._3))
    }
  }

  sealed abstract class HandRank extends Ordered[HandRank] {
     def compare(other: HandRank) : Int = this.handRank - other.handRank
     def handRank : Int
   }

   object HandRank {
     case object HighCard extends HandRank { def handRank = 1 }
     case object Pair extends HandRank { def handRank = 2 }
     case object TwoPair extends HandRank { def handRank = 3 }
     case object ThreeOfAKind extends HandRank { def handRank = 4 }
     case object Straight extends HandRank { def handRank = 5 }
     case object Flush extends HandRank { def handRank = 6 }
     case object FullHouse extends HandRank { def handRank = 7 }
     case object FourOfAKind extends HandRank { def handRank = 8 }
     case object StraightFlush extends HandRank { def handRank = 9 }
   }
}
