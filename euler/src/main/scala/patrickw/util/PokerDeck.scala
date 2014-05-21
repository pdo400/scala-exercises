package patrickw.util

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 10/6/12
 * Time: 10:09 AM
 */
object PokerDeck {
  val cards: Set[Card] = Rank.values.flatMap(rank => Suit.values.map(suit => Card(rank, suit)))

  object Card {
    def apply(s: String) : Option[Card] = {
      s.length match {
        case 2 => Card(s.charAt(0), s.charAt(1))
        case _ => None
      }
    }

    def apply(rank: Char, suit: Char) : Option[Card] = {
      Rank(rank).flatMap(rank => Suit(suit).map(suit => Card(rank, suit)))
    }
  }

  case class Card(rank: Rank, suit: Suit)

  sealed abstract class Suit {}

  object Suit {
    case object Clubs extends Suit
    case object Diamonds extends Suit
    case object Hearts extends Suit
    case object Spades extends Suit

    val values = Set(Clubs, Diamonds, Hearts, Spades)

    def apply(c: Char): Option[Suit] = {
      c match {
        case 'C' => Some(Clubs)
        case 'D' => Some(Diamonds)
        case 'H' => Some(Hearts)
        case 'S' => Some(Spades)
        case _ => None
      }
    }
  }

  sealed abstract class Rank extends Ordered[Rank] {
    def compare(other: Rank) : Int = this.rank - other.rank
    def rank : Int
  }

  object Rank {
    case object Two extends Rank { def rank = 2 }
    case object Three extends Rank { def rank = 3 }
    case object Four extends Rank { def rank = 4 }
    case object Five extends Rank { def rank = 5 }
    case object Six extends Rank { def rank = 6 }
    case object Seven extends Rank { def rank = 7 }
    case object Eight extends Rank { def rank = 8 }
    case object Nine extends Rank { def rank = 9 }
    case object Ten extends Rank { def rank = 10 }
    case object Jack extends Rank { def rank = 11 }
    case object Queen extends Rank { def rank = 12 }
    case object King extends Rank { def rank = 13 }
    case object Ace extends Rank { def rank = 14 }

    val values = Set(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

    def apply(c: Char) : Option[Rank] = {
      c match {
        case '2' => Some(Rank.Two)
        case '3' => Some(Rank.Three)
        case '4' => Some(Rank.Four)
        case '5' => Some(Rank.Five)
        case '6' => Some(Rank.Six)
        case '7' => Some(Rank.Seven)
        case '8' => Some(Rank.Eight)
        case '9' => Some(Rank.Nine)
        case 'T' => Some(Rank.Ten)
        case 'J' => Some(Rank.Jack)
        case 'Q' => Some(Rank.Queen)
        case 'K' => Some(Rank.King)
        case 'A' => Some(Rank.Ace)
        case _ => None
      }
    }
  }
}
