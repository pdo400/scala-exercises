package patrickw.euler

import patrickw.util.combinator.{Y, Memoize}

object P082 {
  private val InputFile = "input/matrix.txt"
  private val size = 80

  def main(args: Array[String]) = {
    val input = io.Source.fromInputStream(getClass.getResourceAsStream(InputFile)).getLines
      .map(_.split(",").map(_.toInt))
      .toArray

    println(f(input))
  }

  def f(input: Array[Array[Int]]): Int = {
    lazy val mspUp: (Int, Int) => Int = Y(Memoize[Int, Int, Int] _ compose {
      (recur: (Int, Int) => Int) => (x, y) => {
        input(y)(x) + {
          if (x == 0)
            0
          else y match {
            case 0 => msp(x - 1, y)
            case _ => math.min(msp(x - 1, y), recur(x, y - 1))
          }
        }
      }
    })

    lazy val mspDown: (Int, Int) => Int = Y(Memoize[Int, Int, Int] _ compose {
      (recur: (Int, Int) => Int) => (x, y) => {
        input(y)(x) + {
          if (x == 0)
            0
          else y match {
            case n if n == size - 1 => msp(x - 1, y)
            case _ => math.min(msp(x - 1, y), recur(x, y + 1))
          }
        }
      }
    })

    lazy val msp: (Int, Int) => Int = Y(Memoize[Int, Int, Int] _ compose {
      (recur: (Int, Int) => Int) => (x, y) => {
        input(y)(x) + {
          if (x == 0)
            0 :: Nil
          else y match {
            case 0 => recur(x - 1, y) :: mspDown(x, y + 1) :: Nil
            case n if n < size - 1 => recur(x - 1, y) :: mspUp(x, y - 1) :: mspDown(x, y + 1) :: Nil
            case _ => recur(x - 1, y) :: mspUp(x, y - 1) :: Nil
          }
        }.min
      }
    })

    (0 to size - 1).map(msp(size - 1, _)).min
  }
}
