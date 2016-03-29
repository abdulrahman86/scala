
val x = 1 :: 2 :: 3 :: 4 :: Nil

Nil.:: (4).:: (3).::(2).::(1)

x.head
x.tail
x.isEmpty

def sumList(l : List[Int]) : Int = {
  l match {
      case Nil => 0
      case p :: ps => p + sumList(ps)
  }
}

sumList(x)

// insertion sort procedure
def isort(xs: List[Int]) : List[Int] = {

  def insert (x: Int, xs: List[Int]) : List[Int] = {
    xs match {
        case Nil => x :: Nil
        case p :: ps =>
          if (p >= x) x :: p :: ps
          else p :: insert (x, ps)
    }
  }

  xs match {
    case Nil => Nil
    case x :: xs => insert(x, isort(xs))
  }
}

// merge sort
def msort(xs: List[Int]) : List[Int] = {

  def merge (x : List[Int], y: List[Int]) : List[Int] = {

    (x, y) match {
          case (Nil, _) => y
          case (_, Nil) => x
          case (xh :: xs, yh :: ys) => {
              if (xh < yh) xh :: merge(xs, y)
              else yh :: merge(x, ys)
          }
    }
  }
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

// merge sort
def msortParametrized[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {

  def merge (x : List[T], y: List[T]) : List[T] = {

    (x, y) match {
      case (Nil, _) => y
      case (_, Nil) => x
      case (xh :: xs, yh :: ys) => {
        if (ord.lt(xh, yh)) xh :: merge(xs, y)
        else yh :: merge(x, ys)
      }
    }
  }
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msortParametrized(fst), msortParametrized(snd))
  }
}

val pair = ("answer", 42)

val (label, value) = pair

scala.Tuple2[Int, String](1, "hi")

msort(List(2, -4, 5, 7, 1))

msortParametrized[Int](List[Int](2, -4, 5, 7, 1))(Ordering.Int)


def testImplicit(implicit a: A): Unit = {
  a.toString
}

class A {
  override def toString = "A"
}

object B {

  implicit object A
}


testImplicit