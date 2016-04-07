val x = Vector[Int](1, 2, 3, 4, 5)

2 +: x

x :+ 2

val xs = Array(1, 2, 3, 44)
xs map(x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper)

s exists (c => c.isUpper)

def unzip[T, U](x: List[(T,U)]) : (List[T], List[U]) = {
  x match {
      case Nil => (Nil, Nil)
      case (th, uh) :: rest => {
        val result = unzip(rest)
        (th :: result._1, uh :: result._2)
      }
  }
}

def flatMap[T, U](op : T => List[U])(x: List[T])=
  ((x map op) foldLeft[List[U]](List():List[U]))((x: List[U], y: List[U]) => x ++ y) //foldLeft(y)(_ ++ _)

def isPrime(n: Int): Boolean =
  (1 to n filter (x => n % x == 0) length)  == 2

isPrime(8)

flatMap[Char, Char]((x: Char) => List('.', x))(List('H', 'e', 'l', 'l', 'o'))


def generatePairs(n: Int, p : ((Int, Int)) => Boolean) =
  (1 until n) flatMap ((i: Int) => (1 until i) map (j => (i, j))) filter(p)


def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
  (for ((xs, ys) <- xs zip ys) yield  (xs * ys)).sum

for (e <- List(1, 2, 3, 4, 5) if e > 3; f <- List(3, 4, 5, 6)) yield (e,f)


def distinct[T](in : List[T]) : List[T] = {
  in match {
      case Nil => Nil
      case x :: xs => {
        val rs = distinct(xs)
        if (rs exists (_ == x )) {
          rs
        }
        else {
          x :: rs
        }
      }
  }
}

distinct(List(6, 2, 5, 2, 4, 5, 6))


val map = Map ("1"-> "US" )

map get "1" get

val totalMap = map withDefaultValue "unkown"

map toList

1 -> 2


class Poly (terms0: Map[Int, Double]) {
  def this (bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)) : (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(1-> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

p1 + p2



val in = Map ('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
'6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")


val charCode = in flatMap (x => x._2 map ((_, x._1)))

def wordCode(word: String) : String =
  word map charCode

wordCode("Java".toUpperCase())

val lengthFunction = (string: String) => string.length

lengthFunction("hi")

object A {
  def apply (x : Int =>  Int) = x(5)
  def apply (x: Int, y: Int) = x + y
  //def apply (x: Int) :Int = x
  def f (implicit a: Int) : Int = a
}
//


//A(2, 3)

A {
  implicit x  => f
}

def f (implicit x: Int) = x + 7

//def func(x: Int) = ???
//
//func {
//  5
//  6
//  7
//}





