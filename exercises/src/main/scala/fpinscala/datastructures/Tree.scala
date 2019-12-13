package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int =
    fold(tree)(a => 1)(1 + _ + _)

  def maximum(tree: Tree[Int]): Int =
    fold(tree)(a => a)(_ max _)

  def depth[A](tree: Tree[A]): Int =
    fold(tree)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = tree match {
    case Leaf(value)         => l(value)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }
}
