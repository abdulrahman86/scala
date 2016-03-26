def mapReduce (op : (Int, Int) => Int)(initVal: Int)(f: Int => Int)(a: Int, b: Int) : Int =
  if (a > b) initVal
  else op (f(a) , mapReduce (op)(initVal) (f)(a+1, b))


def sum (transform : Int => Int)(a: Int, b: Int) = mapReduce ((x, y) => x + y) (0) (transform)(a, b)

def product (op: Int => Int) : (Int, Int)=> Int = mapReduce ((x, y) => x * y)(0)(op)

def cubes (x : Int) = sum (x => x * x * x) (1, x)

val x = cubes(5)



