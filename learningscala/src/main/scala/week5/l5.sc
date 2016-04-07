import scala.annotation.tailrec

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


def testImplicit(implicit a: A[Int]): String = {
  a.toString
}

abstract class A[T] {

}

implicit object IntA extends A[Int] {

  override def toString = "a"
}

//implicit object IntB extends A[Int] {
//
//  override def toString = "b"
//}

def map[T, U](transform: T => U)(in: List[T]) : List[U] = {

  @tailrec
  def mapTailRecur(acc: List[U])(in: List[T]) : List[U] = {
    in match {
      case Nil => acc
      case (x :: xs) =>  mapTailRecur(acc :+ transform(x))(xs)
    }
  }

  mapTailRecur(List())(in)
}

def mapTail[T, U](transform: T => U)(in: List[T]) : List[U] = {
  in match {
    case Nil => Nil
    case (x :: xs) => transform(x) :: map (transform)(in)
  }
}

def filter[T](p : T => Boolean)(in:List[T]) : List[T] = {
  @tailrec
  def filterTailRecur(acc: List[T])(in:List[T]) : List[T] = {
    in match {
        case Nil => acc
        case x :: xs => {
          if (p (x)) filterTailRecur(acc :+ x)(xs)
          else filterTailRecur(acc)(xs)
        }
    }
  }
  filterTailRecur(List())(in)
}

def filterNot[T](p: T => Boolean)(in: List[T]) : List[T] = {
  filter[T](x => !p(x))(in)
}

def partitiion[T](p: T => Boolean)(in: List[T]) : (List[T], List[T]) = {
  (filter(p)(in), filterNot(p)(in))
}

partitiion[Int](x => x > 0)(List(-1, 1, 4, 5, -6))

def pack[T] (in: List[T]) : List[List[T]] = {
  in match {
      case Nil => Nil
      case (x :: xs) => {
        val result = pack(xs)
        result match {
            case Nil => List(List(x))
            case (r :: rs) => {
              if (r.head == x) {
                (x :: r) :: rs
              }
              else {
                List(x) :: result
              }
            }
        }
      }
  }
}

def encode[T](in: List[T]) : List[(T, Int)] = {
  pack(in) map (x => (x.head, x.length))
}

def foldLeft[T, U] (op: (T, U) => U)(base: U)(in: List[T]) : U = {
  in match {
      case Nil => base
      case (x :: xs) => {
        foldLeft(op)(op(x, base))(xs)
      }
  }
}

def foldRight[T, U] (op: (T, U) => U)(base: U)(in: List[T]) : U = {
  in match {
    case Nil => base
    case (x :: xs) => {
      op(x, foldRight(op)(base)(xs))
    }
  }
}

def concat[T](a: List[T], b: List[T]) = foldRight[T, List[T]](_ :: _)(b)(a)

pack[String](List("a", "a", "a", "b", "c", "c", "c", "a"))
encode[String](List("a", "a", "a", "b", "c", "c", "c", "a"))

foldLeft[Int, Int](_ + _)(0)(List(1, 2, 3, 4))

foldRight[Int, Int](_ + _)(0)(List(1, 2, 3, 4))

concat(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10))
