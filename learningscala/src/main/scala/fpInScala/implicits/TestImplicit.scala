package fpInScala.implicits

object Implicits1 {

  implicit def a: String = "hello"

  trait ContextBound[T] {
    def string1 = "contextBound"
  }

  implicit object ContextBound1 extends ContextBound[TestContextBound] {

  }
}

trait Implicits2 {
  implicit def b: String = "bye"
}

trait ViewBound[T] {
  def string1 = "viewBound"
}


class TestContextBound {

}


class TestViewBound extends ViewBound[Char] {

}


object Test {

  import Implicits1._

  def string(implicit a: String) =  a

  def string2: String = string

  def testViewBound[C <% ViewBound[_]](c: C) = c.string1

  def testContextBound[C: ContextBound](c: C) = {
    implicitly[ContextBound[C]].string1
  }

}
