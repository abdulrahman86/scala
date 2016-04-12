package fpInScala.c3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def reverse[A] (in: List[A]) : List[A] =
    foldLeft(in, Nil : List[A])((a, b) => Cons(a, b))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldRight(l, (x : B) => x) ((x, f1) => (y) => f1(f(x, y)))(z)

  def foldRWithFoldL[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(List.reverse(l), z)(f)

  def append[A] (l1: List[A], l2 : List[A] ) : List[A] =
    foldRight(l1, l2)(Cons(_ , _))

  def concat[A] (l: List[List[A]]) : List[A] =
    foldRight(l, Nil: List[A])(append(_, _))

  def tail[A](l : List[A]) : List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => t
  }


}