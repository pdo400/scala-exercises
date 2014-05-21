package patrickw.util

/**
 * Created by IntelliJ IDEA.
 * User: patrickw
 * Date: 10/12/12
 * Time: 12:05 PM
 */
sealed abstract class LatticeDirection {
  def unary_! : LatticeDirection
  def isPerpendicular(to: LatticeDirection) = to != this && to != !this
  def apply[A](from: (A, A))(implicit num: Integral[A]): (A, A) = apply(from, num.one)
  def apply[A](from: (A, A), steps: A)(implicit num: Integral[A]): (A, A)
}

object LatticeDirection {
  case object Up extends LatticeDirection {
    def unary_! = Down
    def apply[A](from: (A, A), steps: A)(implicit num: Integral[A]): (A, A) = (from._1, num.plus(from._2, steps))
  }
  case object Down extends LatticeDirection {
    def unary_! = Up
    def apply[A](from: (A, A), steps: A)(implicit num: Integral[A]): (A, A) = (from._1, num.minus(from._2, steps))
  }
  case object Left extends LatticeDirection {
    def unary_! = Right
    def apply[A](from: (A, A), steps: A)(implicit num: Integral[A]): (A, A) = (num.minus(from._1, steps), from._2)
  }
  case object Right extends LatticeDirection {
    def unary_! = Left
    def apply[A](from: (A, A), steps: A)(implicit num: Integral[A]): (A, A) = (num.plus(from._1, steps), from._2)
  }

  val values: Set[LatticeDirection] = Set(Up, Down, Left, Right)
}
