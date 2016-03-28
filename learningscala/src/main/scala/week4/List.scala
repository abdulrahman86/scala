package week4

import scala.Boolean


trait List[+T] {

  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](x: U) : List[U]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

  def isEmpty = false

  def prepend[U >: T] (x: U) = new Cons(x, this)

  override def toString = "[" + head + ", " + tail + "]"

}

object Nil extends List[Nothing] {

  def isEmpty = true


  def head = throw new NoSuchFieldException("Nil.head")

  def tail = throw new NoSuchFieldException("Nil.tail")

  def prepend[U] (x: U) = new Cons(x, this)

  override def toString = "[]"
}

object List {
  def apply[T](values: T*): List[T] =
    if (values.isEmpty) Nil
    else new Cons[T](values.head, List.apply(values.tail: _*))
}
