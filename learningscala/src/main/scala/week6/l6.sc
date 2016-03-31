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

