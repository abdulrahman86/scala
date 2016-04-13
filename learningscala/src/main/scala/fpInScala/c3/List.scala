package fpInScala.c3

import scala.math.Ordering.Implicits._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

sealed trait Tree[+T]
final object Empty extends Tree[Nothing]
final case class Leaf[A](value: A) extends Tree[A]
final case class Branch[A](elem: A, left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A, B](t: Tree[A])(init: B)(op: (B, A, B) => B) : B = t match {
    case Empty => init
    case Leaf(x) => op(init, x, init)
    case Branch(x, left, right) => op((fold(left)(init)(op)), x, fold(right)(init)(op))
  }

  def size[A](t: Tree[A]) : Int = fold[A, Int] (t)(0)((left, elem, right) => left + 1 + right)

  def max(t: Tree[Int]) : Int = fold[Int, Int](t)(0)((left, elem, right) => Math.max(elem, Math.max(left, right)))

  def depth[A](t: Tree[A]) : Int = fold[A, Int](t)(0)((left, elem, right) => 1 + (Math.max(left, right)))

  def map[A, B](t: Tree[A])(f : A => B) : Tree[B] = fold[A, Tree[B]](t)(Empty)((left, elem, right) => Branch(f(elem), left, right))
}

object BinaryTree {

  def insert[ A : Ordering] (t : Tree[A])(implicit elem : A): Tree[A] = {
    val order = implicitly[Ordering[A]]
    t match {
      case Empty => Leaf(elem)
      case Leaf(x) if (elem == x) => t
      case Leaf(x) if (order.lt(elem, x)) => new Branch(elem, Empty, new Leaf(x))
      case Leaf(x)  => new Branch(elem, new Leaf(x), Empty)
      case Branch(x, left, right) if (elem == x) => t
      case Branch(x, left, right) if (order.lt(elem, x)) => new Branch(x, insert(left), right)
      case Branch(x, left, right) =>   new Branch(x, left, insert(right))
    }
  }

  def apply[A: Ordering](as: A*): Tree[A] =
    as.tail.foldLeft[Tree[A]](Leaf(as.head))((t, elem) => BinaryTree.insert(t)(implicitly[Ordering[A]], elem))
}


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