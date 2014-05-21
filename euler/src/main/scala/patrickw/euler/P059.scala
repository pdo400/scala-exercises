package patrickw.euler

import patrickw.util.combinator.{Y, Memoize}
import collection.mutable.ArrayBuffer
import java.io.InputStreamReader
import util.matching.Regex
import patrickw.util.{ProductStream, Pow, Permutations}

object P059 {
  private val encryptedFile = "input/cipher1.txt"
  private val wordsFile = "input/words.txt"
  private val cipherLength = 3

  private val letterFrequency = " ETAONRISHDLFCMUGYPWBVKJXQZ".toLowerCase.toCharArray

  def main(args: Array[String]) = {
    val input = io.Source.fromInputStream(getClass.getResourceAsStream(encryptedFile)).getLines().map(_.toInt).toSeq
    val words = io.Source.fromInputStream(getClass.getResourceAsStream(wordsFile)).getLines().toSet

    val x = f(input, words)
    println(x.map{ case (a, b, c) => (a.toChar, b.toChar, c.toChar) })
    println(x.map{ case (a, b, c) => a + b + c })
  }

  def read(reader: InputStreamReader, acc: ArrayBuffer[Int] = ArrayBuffer[Int](), current: Int = 0): Array[Int] = {
    reader.ready() match {
      case false => acc.toArray
      case true => reader.read() match {
        case ',' => read(reader, acc += current)
        case n => read(reader, acc, 10 * current + (n - '0'))
      }
    }
  }

  def f(input: Seq[Int], words: Set[String]): Option[(Int, Int, Int)] = {
    val inputWithIndex = input.zipWithIndex.map{ case (a, b) => (a, b % cipherLength) }
    val frequencyMaps = inputWithIndex.foldLeft((0 until cipherLength).map(_ -> Map[Int, Int]()).toMap) {
      case (maps, (char, index)) => maps + (index -> (maps(index) + (char -> (maps(index).getOrElse(char, 0) + 1))))
    }.map {
      case (index, map) => (index, map.toList.sortBy(-_._2).map(_._1))
    }

    ProductStream(frequencyMaps(0), frequencyMaps(1), frequencyMaps(2)).flatMap {
      case (e1, e2, e3) =>
        ProductStream(letterFrequency, letterFrequency, letterFrequency).map {
          case (g1, g2, g3) => (e1 ^ g1, e2 ^ g2, e3 ^ g3)
        }.filter {
          case (c1, c2, c3) => isCypherLetter(c1) && isCypherLetter(c2) && isCypherLetter(c3)
        }
    } .find { case (c1, c2, c3) => {
      val (good, total) = inputWithIndex.map {
        case (e, 0) => (e ^ c1).toChar.toUpper
        case (e, 1) => (e ^ c2).toChar.toUpper
        case (e, 2) => (e ^ c3).toChar.toUpper
      } .mkString.split("[^A-Z']+").foldLeft((0, 0)) {
        case ((good, total), next) => next.length match {
          case 0 => (good, total)
          case _ => (if (words.contains(next)) good + 1 else good, total + 1)
        }
      }
      1.0 * good / total > 0.5
    }}
  }

  def isCypherLetter(c: Int) = 'a' <= c && c <= 'z'
}
