import scala.util.control.NonFatal

object TryMonad {

  object Try {
    def apply[A](expr: => A) = try Success(expr) catch { case NonFatal(e) => Failure(e) }
  }

  sealed trait Try[+A] {
    def flatMap[B](f: A => Try[B]): Try[B] = this match {
      case Success(x) => try f(x) catch { case NonFatal(e) => Failure(e) }
      case failure: Failure => failure
    }

    def map[B](f: A => B): Try[B] = this match {
      case Success(x) => try Success(f(x)) catch { case NonFatal(e) => Failure(e) }
      case failure: Failure => failure
    }

//    def withFilter(f: A => Boolean) = this match {
//      case Success(x) =>
//    }
  }
  case class Success[A](value: A) extends Try[A]
  case class Failure(e: Throwable) extends Try[Nothing]
}

object Assoc {
  import TryMonad._

  def l0[A, B, C](m: Try[A], f: A => Try[B], g: B => Try[C]) = m flatMap f flatMap g

  def l1[A, B, C](m: Try[A], f: A => Try[B], g: B => Try[C]) =
    (m match {
      case Success(a) => try f(a) catch { case NonFatal(e) => Failure(e) }
      case failure: Failure => failure
    }) match {
      case Success(b) => try g(b) catch { case NonFatal(e) => Failure(e) }
      case failure: Failure => failure
    }

  def l2[A, B, C](m: Try[A], f: A => Try[B], g: B => Try[C]) =
    m match {
      case Success(a) => (try f(a) catch { case NonFatal(e) => Failure(e) }) match {
        case Success(b) => try g(b) catch { case NonFatal(e) => Failure(e) }
        case failure: Failure => failure
      }
      case failure: Failure => failure
    }

  def r0[A, B, C](m: Try[A], f: A => Try[B], g: B => Try[C]) = m flatMap (a => f(a) flatMap g)

  def r1[A, B, C](m: Try[A], f: A => Try[B], g: B => Try[C]) =
    m flatMap (a => f(a) match {
      case Success(b) => try g(b) catch { case NonFatal(e) => Failure(e) }
      case failure: Failure => failure
    })

  def r2[A, B, C](m: Try[A], f: A => Try[B], g: B => Try[C]) =
    m match {
      case Success(a) =>
        try f(a) match {
          case Success(b) => try g(b) catch { case NonFatal(e) => Failure(e) }
          case failure: Failure => failure
        } catch {
          case NonFatal(e) => Failure(e)
        }
      case failure : Failure => failure
    }
}

object LeftUnit {
  import TryMonad._

  def lu0[A, B](x: A, f: A => Try[B]) = Try(x) flatmap f

  def lu1[A, B](x: A, f: A => Try[B]) = Try(x) match {
    case Success(a) => try f(x) catch { case NonFatal(e) => Failure(e) }
    case failure: Failure => failure
  }
}


object RightUnit {
  import TryMonad._

  def ru0[A](m: Try[A]) = m flatmap Try

  def ru1[A](m: Try[A]) = m match {
    case Success(a) => try Try(a) catch { case NonFatal(e) => Failure(e) }
    case failure : Failure => failure
  }
}
