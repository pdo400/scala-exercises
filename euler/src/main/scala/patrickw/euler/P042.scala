package patrickw.euler

import patrickw.util.{Polygonal, LetterValues}

object P042 {
  private val InputFile = "input/words.txt"

  def main(args: Array[String]) = {
    var acc = 0
    f() foreach ({ case (s, v) => acc += 1; println(v + " " + s) })
    println
    println(acc)
  }

  def f(): Iterator[(String, Int)] = {
    val triangulars = Polygonal(3)
    io.Source.fromInputStream(getClass.getResourceAsStream(InputFile)).getLines
      .map(s => s -> LetterValues(s).sum)
      .filter({ case (s, v) => triangulars.isSolution(v) })
  }
}
