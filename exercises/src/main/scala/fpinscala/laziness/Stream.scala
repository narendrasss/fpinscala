package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _          => List()
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1)          => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _                      => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWith[B, C](as: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, as)) {
      case ((Cons(h1, t1), Cons(h2, t2))) => Some((f(h1(), h2()), (t1(), t2())))
      case _                              => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](ss: => Stream[B]): Stream[B] =
    foldRight(ss)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constant[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def from(n: Int): Stream[Int] =
    unfold(n)(num => Some((num, num + 1)))

  def fibs(): Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => empty[A]
      case Some((h, s)) => cons(h, unfold(s)(f))
    }
}
