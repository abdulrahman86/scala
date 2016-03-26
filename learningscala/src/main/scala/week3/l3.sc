import week3.{Empty, Rational}

val x = new Rational(4, 2)

def x1 = 2
new Rational(2)

x.numer
x.denom

x  < - new Rational(11, 5)

def addRat(x: Rational, y: Rational) : Rational =
  new Rational(x.numer * y.denom + y.numer * x.denom,
    x.denom * y.denom)

x.toString

val tree = Empty incl 3 incl 4 incl 2 incl 6

val tree2 = Empty incl 1 incl 2 incl 10 incl 12

tree union tree2

def error(x: Int) =
  if (x > 1) x
  else throw new Error("Error")

error (0)



