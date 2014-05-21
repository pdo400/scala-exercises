package patrickw.util.combinator

object Y {
  def apply[T, R](f: (T => R) => (T => R)): (T => R) = {
    lazy val yf: T => R = f(yf(_))
    yf
  }

  def apply[T1, T2, R](f: ((T1, T2) => R) => ((T1, T2) => R)): ((T1, T2) => R) = {
    lazy val yf: (T1, T2) => R = f(yf(_, _))
    yf
  }

  def apply[T1, T2, T3, R](f: ((T1, T2, T3) => R) => ((T1, T2, T3) => R)): ((T1, T2, T3) => R) = {
    lazy val yf: (T1, T2, T3) => R = f(yf(_, _, _))
    yf
  }
}
