package week7

/**
  * Created by asattar on 2016-04-05.
  */
trait Stream[+T] {

  def head : T
  def tail : Stream[T]
}

case object Empty extends Stream[Nothing] {


  override  def head = throw new Error("Head of empty stream")

  override  def tail = throw new Error("Head of empty stream")
}


case class NonEmptyStream[T](val head: T, val tailParam: () => Stream[T]) extends Stream[T] {

  override  def tail = tailParam()
}

object Stream {

  def #::[T](x: T, y: => Stream[T]) : Stream[T] =
    new NonEmptyStream(x, () => y)

  def map[A,B](f : A => B) (stream : Stream[A]) : Stream[B] =
    stream match {
        case Empty => Empty
        case NonEmptyStream(head, tail) => #::(f(head), map (f)(stream.tail))
    }

  def streamRange(start: Int, end: Int) : Stream[Int] =
    if (start > end) Empty
    else #::(start, streamRange(start + 1, end))

  def from(x: Int) : Stream[Int] =
    #:: (x, from(x + 1))

  def take[T](n : Int)(in : Stream[T]) : Stream [T] =
    n match {
        case 0 => Empty
        case x => #:: (in.head, take(x-1)(in.tail))
    }

  def toList[T](in : Stream[T]) : List[T] =
    in match {
        case Empty => List()
        case NonEmptyStream(x, tail) => x :: toList(in.tail)
    }

  def filter[T](p : T => Boolean)(in : Stream[T]) : Stream[T] =
    in match {
      case Empty => Empty
      case NonEmptyStream(head, tail) =>
        p(head) match {
          case true => #:: (head, filter (p)(in.tail))
          case false => filter(p)(in.tail)
        }
    }

}





