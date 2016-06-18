package fpInScala.c5

sealed trait Stream[+A]

case object Empty extends Stream[Nothing]

case class Cons[+A] (h : A, t : () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A] (hd: => A, tl : => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(head, () => tail)
  }

  def headOption[A] (in: Stream[A]) : Option[A] = in match {
    case Cons(h, t) => Some(h)
    case _ => None
  }

  def tail[A] (in: Stream[A]) : Stream[A] = in match {
    case Cons(h, t) => t()
    case _ => Empty
  }

  def toList[A](in: Stream[A]) : List[A] = in match {
    case Empty => List()
    case Cons(h, t) => h :: toList(t())
  }

  def apply[A](as : A*) : Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  def taken[A](n: Int)(in: Stream[A]) : Stream[A] = in match {
    case Cons(h, t) if n > 0 => cons(h, taken (n-1) (t()))
    case _  => Empty
  }

  def dropn[A](n: Int)(in: Stream[A]) : Stream[A] = in match {
    case Cons(h, t) if n > 0 => dropn (n-1) (t())
    case x => x
  }

  def takeWhile[A](p: A => Boolean)(in: Stream[A]) : Stream[A] = in match {
    case Cons(h, t) if p(h) => cons(h, takeWhile (p) (t()))
    case _  => Empty
  }

  def foldRight[A, B](z: B)(f: (A, => B) => B)(in : Stream[A]) : B = in match {
    case Empty => z
    case Cons(h, t) => f(h, foldRight(z)(f)(t()))
  }

  def exists[A](p: A => Boolean)(in: Stream[A]) =
    foldRight[A, Boolean](false)((a, b) => p(a) || b)(in)

  def forAll[A](p : A => Boolean)(in : Stream[A]) =
    foldRight[A, Boolean](true)((x, y) => p(x) && y)(in)

  def takeWhile2[A](p: A => Boolean)(in: Stream[A]) : Stream[A] =
    foldRight[A, Stream[A]](Empty)((x,y) => if (p(x)) cons(x, y) else Empty)(in)


  def headOption2[A] (in: Stream[A]) : Option[A] =
    foldRight[A, Option[A]](None)((x, y) => Some(x))(in)

  def map[A, B](f: A => B)(in: Stream[A]) =
    foldRight[A, Stream[B]](Empty)((x, y) => cons(f(x), y))(in)

  def filter[A](p : A => Boolean)(in: Stream[A]) =
    foldRight[A, Stream[A]](Empty)((x, y) => if (p(x)) cons(x, y) else y)(in)

  def append[A](in1: => Stream[A])(in2: => Stream[A]) : Stream[A] =
    foldRight[A, Stream[A]](in2)((x, y) => cons(x, y))(in1)


  def flatMap[A](in: Stream[Stream[A]]) =
    foldRight[Stream[A], Stream[A]](Empty)((x, y) => append(x)(y))(in)

  def constant[A](a: A) : Stream[A] =
    cons(a, constant(a))

  def from(n: Int) : Stream[Int] =
    cons(n, from(n+1))

  def add(in1: Stream[Int])(in2: Stream[Int]) : Stream[Int] = (in1, in2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => cons(h1 + h2, add(t1())(t2()))
    case _ => Empty
  }

  def fibs: Stream[Int] = cons(0 , cons(1,add(fibs)(tail(fibs))))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,b)) => cons(a, unfold(b)(f))
    case None => Empty
  }

  def fibs2: Stream[Int] =
    unfold[Int, (Stream[Int], Stream[Int])](constant(0), constant(1))(x => Some(Stream.headOption(x._1).get, (x._2, constant(Stream.headOption(x._1).get + Stream.headOption(x._2).get))))


  def map2[A, B](f: A => B)(in: Stream[A]) =
    unfold[B, Stream[A]](in)(x => x match {
      case Empty => None
      case Cons(h, t) => Some(f(h), t())
    })

  def taken2[A](n: Int)(in: Stream[A]) : Stream[A] =
    unfold[A, (Int, Stream[A])]((n, in))(x => x match {
      case (n, Cons(h, t)) if n > 0 => Some(h, (n-1, t()))
      case _ => None
    })

  def takeWhile3[A](p: A => Boolean)(in: Stream[A]) : Stream[A] =
    unfold[A, Stream[A]](in){
      case Cons(h, t) if p(h) => Some(h, t())
      case _ => None
    }

  def zipAll[A, B](in1: Stream[A], in2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((in1, in2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1), Some(h2)), (t1(), t2()))
      case (_, Cons(h2, t2)) => Some((None, Some(h2)), (Empty, t2()))
      case (Cons(h1, t1), _) => Some((Some(h1), None), (t1(), Empty))
      case _ => None
    }

  def tails[A](in: Stream[A]): Stream[Stream[A]] =
    unfold[Stream[A],Stream[A]](in)({
      case Empty => None
      case x => Some((x, tail(x)))
    })

  def scan[A, S](z: S)(f : (A, =>S) => S)(in: Stream[A]): Stream[S] =
    unfold[S, Stream[A]](in){
      case Empty => None
      case x => Some((foldRight[A, S](z)(f)(x), tail(x)))
    }

  def scan2[A, S](z: S)(f : (A, =>S) => S)(in: Stream[A]): Stream[S] = in match {
    case Empty => Stream(z)
    case x =>  cons(foldRight[A, S](z)(f)(x), scan2(z)(f)(tail(x)))
  }

  def tails2[A](in: Stream[A]): Stream[Stream[A]] =
    scan2[A, Stream[A]](Empty)((x, y) => cons(x, y))(in)
}