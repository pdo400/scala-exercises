package patrickw.util.combinator

import collection.mutable

object Memoize {
  def apply[T, R](f: T => R): T => R = {
    val m = mutable.HashMap[T, R]()
    (t: T) => m.getOrElseUpdate(t, f(t))
  }

  def apply[T1, T2, R](f: (T1, T2) => R): (T1, T2) => R = {
    val m = mutable.HashMap[(T1, T2), R]()
    (t1: T1, t2: T2) => m.getOrElseUpdate((t1, t2), f(t1, t2))
  }

  def apply[T1, T2, T3, R](f: (T1, T2, T3) => R): (T1, T2, T3) => R = {
    val m = mutable.HashMap[(T1, T2, T3), R]()
    (t1: T1, t2: T2, t3: T3) => m.getOrElseUpdate((t1, t2, t3), f(t1, t2, t3))
  }

  def firstArg[T1, T2, R](f: (T1, T2) => R): (T1, T2) => R = {
    val m = mutable.HashMap[T1, R]()
    (t1: T1, t2: T2) => m.getOrElseUpdate(t1, f(t1, t2))
  }

  def firstArg[T1, T2, T3, R](f: (T1, T2, T3) => R): (T1, T2, T3) => R = {
    val m = mutable.HashMap[T1, R]()
    (t1: T1, t2: T2, t3: T3) => m.getOrElseUpdate(t1, f(t1, t2, t3))
  }

  def when[T, R](filter: T => Boolean)(f: T => R): T => R =
    IfElse(filter, apply(f), f)

  def when[T1, T2, R](filter: (T1, T2) => Boolean)(f: (T1, T2) => R): (T1, T2) => R =
    IfElse(filter, apply(f), f)

  def when[T1, T2, T3, R](filter: (T1, T2, T3) => Boolean)(f: (T1, T2, T3) => R): (T1, T2, T3) => R =
    IfElse(filter, apply(f), f)
}
