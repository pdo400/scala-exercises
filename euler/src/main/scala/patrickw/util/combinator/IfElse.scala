package patrickw.util.combinator

object IfElse {
  def apply[T, R](predicate: T => Boolean, _true: T => R, _false: T => R): T => R =
    t => (if (predicate(t)) _true else _false)(t)

  def apply[T1, T2, R](predicate: (T1, T2) => Boolean, _true: (T1, T2) => R, _false: (T1, T2) => R): (T1, T2) => R =
    (t1, t2) => (if (predicate(t1, t2)) _true else _false)(t1, t2)

  def apply[T1, T2, T3, R](predicate: (T1, T2, T3) => Boolean, _true: (T1, T2, T3) => R, _false: (T1, T2, T3) => R): (T1, T2, T3) => R =
    (t1, t2, t3) => (if (predicate(t1, t2, t3)) _true else _false)(t1, t2, t3)
}
