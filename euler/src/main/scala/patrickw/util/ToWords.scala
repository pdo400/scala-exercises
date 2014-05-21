package patrickw.util

object ToWords {
  def apply(n: Int, acc: StringBuilder = new StringBuilder()): StringBuilder = {
    def f(n: Int): StringBuilder =
      n match {
        case 0 => acc ++= ""
        case 1 => acc ++= "one"
        case 2 => acc ++= "two"
        case 3 => acc ++= "three"
        case 4 => acc ++= "four"
        case 5 => acc ++= "five"
        case 6 => acc ++= "six"
        case 7 => acc ++= "seven"
        case 8 => acc ++= "eight"
        case 9 => acc ++= "nine"
        case 10 => acc ++= "ten"
        case 11 => acc ++= "eleven"
        case 12 => acc ++= "twelve"
        case 13 => acc ++= "thirteen"
        case 14 => acc ++= "fourteen"
        case 15 => acc ++= "fifteen"
        case 16 => acc ++= "sixteen"
        case 17 => acc ++= "seventeen"
        case 18 => acc ++= "eighteen"
        case 19 => acc ++= "nineteen"
        case x if x < 30 => acc ++= "twenty"; f(x % 10)
        case x if x < 40 => acc ++= "thirty"; f(x % 10)
        case x if x < 50 => acc ++= "forty"; f(x % 10)
        case x if x < 60 => acc ++= "fifty"; f(x % 10)
        case x if x < 70 => acc ++= "sixty"; f(x % 10)
        case x if x < 80 => acc ++= "seventy"; f(x % 10)
        case x if x < 90 => acc ++= "eighty"; f(x % 10)
        case x if x < 100 => acc ++= "ninety"; f(x % 10)
        case x if x < 1000 => {
          f(x / 100);
          acc ++= "hundred"
          x % 100 match {
            case 0 => f(0)
            case x => acc ++= "and"; f(x)
          }
        }
        case x if x < 1000000 => f(x / 1000); acc ++= "thousand"; f(x % 1000)
        case x if x < 1000000000 => f(x / 1000000); acc ++= "million"; f(x % 100000)
        case _ => throw new IllegalArgumentException("n must be < 1000000000")
      }
    f(n)
  }
}
