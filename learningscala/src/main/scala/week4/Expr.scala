package week4

/**
  * Created by asattar on 2016-03-28.
  */

/*
Problem with Object Oriented Decomposition
1)To introduce a new method all the class hierarchichies have to be touched
2)Does not go well with non-local simplification. Back to square 1 where we have to check for classification.
 */
trait Expr {

  def numValue: Int
  def leftOp : patternMaching.Expr
  def rightOp : patternMaching.Expr
  def eval : Int
}

class Number(val n: Int) extends patternMaching.Expr {
  def numValue = n
  def leftOp = throw new Error("Number.leftOp")
  def rightOp = throw new Error("Number.rightOp")
  def eval = numValue
}

class Sum(val e1: patternMaching.Expr, val e2: patternMaching.Expr) extends patternMaching.Expr {
  def numValue = throw new Error("Sum.numValue")
  def leftOp = e1
  def rightOp = e2
  def eval = e1.eval + e2.eval
}
