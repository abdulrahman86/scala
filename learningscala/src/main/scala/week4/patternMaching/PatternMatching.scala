package week4.patternMaching

/**
  * Created by asattar on 2016-03-28.
  */

/**
  * 1) If lean towards class creation, oo decomposition preferreed
  * 2) If lean towards lots of new method then pattern matching preferred.
  */
trait Expr {

  def eval: Int = this match {

    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}
case class Number(val n: Int) extends Expr
case class Sum(val e1: Expr, val e2: Expr) extends Expr

object Expr {
  def eval(e: Expr): Int = e match {

      case Number(n) => n
      case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String = e match{

      case Number(n)  => n.toString
      case Sum(e1, e2) => "(+ " + show(e1) + " " + show(e2) + " )"
  }
}