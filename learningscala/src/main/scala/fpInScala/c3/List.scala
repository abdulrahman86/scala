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

  def map[A,B](l: List[A])(implicit f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(tail(l)))
  }


  def flatMap[A,B](l: List[A])(implicit f: A => List[B]): List[B] =
    concat(map(l))

  def filter[A](l: List[A])(implicit f: A => Boolean): List[A] =
    flatMap(l)(x => if(f(x)) List(x) else List())

  def zipWith[A, B, C](l1: List[A])(l2: List[B])(implicit f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case ((Cons(h1, t1), Cons(h2, t2))) => Cons(f(h1,h2), zipWith(t1)(t2))
  }
}