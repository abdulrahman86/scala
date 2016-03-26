import scala.annotation.tailrec

def square (x:Double) = x * x

def sumOfSquares(x:Double, y:Double) = square(x) + square(y)

square(2)
sumOfSquares(3,4)

def conditional() =
  if (1 > 2)
     1
  else
      2

conditional()

val x = conditional()

def loop : Boolean = loop

def x1  = 5

x1

def sqrt (x: Double) : Double = {
  def sqrtHelper(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtHelper(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double): Boolean =
    Math.abs(guess * guess - x) <= 0.001

  def improve(guess: Double, x: Double): Double =
    (guess + x / guess) / 2

  sqrtHelper(1, x)

}
def factorial (n: Int) : Int = {

  def factorialHelper (n: Int, acc:Int) : Int =
    if (n == 0) acc
    else factorialHelper(n-1, acc * n)

  factorialHelper(n, 1)
}

val x2 = sqrt(2)
x2

factorial(5)
