import week4._

def singleton[T](elem: T) = new Cons[T](elem, Nil)

def nthElem[T](n: Int, list: List[T]) : T =
  if (n == 0) list.head
  else nthElem(n-1, list.tail)


True && False || False

Zero

new Succ(Zero) + new Succ(new Succ(Zero))

List(1, 2, 3, 4)

var v1 = new A[BSuper, BDerived1, BDerived1]
var v2 = new A[BSuper, BSuper, BDerived1]
var v3 = new A[BSuper, BSuper, BDerived2]

v1 = v2

var v4 = List(v1, v2)
var v5 = new Cons(v3, v4)
var v6 = List[BDerived1]

