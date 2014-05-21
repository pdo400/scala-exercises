package patrickw.euler

import patrickw.util.LetterValues

object P022 {
  private val InputFile = "input/P022.txt"

  def main(args: Array[String]) =
    println(f())

  def f(): Long =
    io.Source.fromInputStream(getClass.getResourceAsStream(InputFile)).getLines.toSeq.sorted.foldLeft(0L) {
      var i = 0
      (acc, name) => {
        i += 1;
        acc + i * LetterValues(name).sum
      }
    }

}
