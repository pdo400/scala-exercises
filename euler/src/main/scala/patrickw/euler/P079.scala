package patrickw.euler

import scalaz._
import Scalaz._
import patrickw.util.{Combinations, Integers, PythagoreanTriple}
import annotation.tailrec


object P079 {
  protected val inputFile = "input/keylog.txt"
  protected val inputLength = 3
  protected val input = io.Source.fromInputStream(getClass.getResourceAsStream(inputFile)).getLines().toSeq

  def main(args: Array[String]) {
    println(find())
  }

  @tailrec
  def find(length: Int = inputLength): String = {
    val combinations = Combinations((1 to length), inputLength).toSeq

    @tailrec
    def f(inputs: Seq[String] = input, partials: List[Map[Int, Char]] = List(Map.empty)): List[Map[Int, Char]] = {
      if (partials.isEmpty)
        partials
      else inputs match {
        case Seq() => partials
        case Seq(input, tail @ _*) => {
          f(tail, partials.flatMap {
            p => combinations.flatMap {
              c => c.zip(input).foldLeft(Some(p): Option[Map[Int, Char]]) {
                case (None, _) => None
                case (Some(partial), (position, char)) => {
                  partial.get(position).cata(
                    existing => if (existing == char) Some(partial) else None,
                    Some(partial + (position -> char)))
                }
              }
            }
          })
        }
      }
    }

    f() match {
      case Nil => find(length + 1)
      case l => l.map(m => (1 to length).map(n => m.get(n).getOrElse('?')).mkString).mkString(", ")
    }
  }
}
